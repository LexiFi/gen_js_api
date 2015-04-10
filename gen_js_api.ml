(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by Alain Frisch and LexiFi.                             *)


open Location
open Asttypes
open Parsetree
open Longident
open Ast_helper

(** Errors *)

type error =
  | Expression_expected
  | Identifier_expected
  | Invalid_expression
  | Multiple_binding_declarations
  | Binding_type_mismatch
  | Cannot_parse_type
  | Cannot_parse_sigitem
  | Setter_name
  | Unit_not_supported_here
  | Non_constant_constructor_in_enum
  | Default_case_in_enum
  | Multiple_default_case_in_enum

exception Error of Location.t * error

let has_attribute key attrs =
  let p ({txt; loc = _}, _) = txt = key in
  List.exists p attrs

let error loc err = raise (Error (loc, err))

let print_error ppf = function
  | Expression_expected ->
      Format.fprintf ppf "Expression expected"
  | Identifier_expected ->
      Format.fprintf ppf "String literal expected"
  | Invalid_expression ->
      Format.fprintf ppf "Invalid expression"
  | Multiple_binding_declarations ->
      Format.fprintf ppf "Multiple binding declarations"
  | Binding_type_mismatch ->
      Format.fprintf ppf "Binding declaration and type are not compatible"
  | Cannot_parse_type ->
      Format.fprintf ppf "Cannot parse type"
  | Cannot_parse_sigitem ->
      Format.fprintf ppf "Cannot parse signature item"
  | Setter_name ->
      Format.fprintf ppf "Setter with implicit name must start with 'set_'"
  | Unit_not_supported_here ->
      Format.fprintf ppf "Unit not supported in this context"
  | Non_constant_constructor_in_enum ->
      Format.fprintf ppf "Constructors in enums cannot take arguments"
  | Default_case_in_enum ->
      Format.fprintf ppf "Default constructor in enums unique argument must be of type int or string"
  | Multiple_default_case_in_enum ->
      Format.fprintf ppf "At most one default constructor is supported in enums for each type"

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) -> Some (Location.error_of_printer loc print_error err)
      | _ -> None
    )


(** AST *)

type enum_params =
  {
    enums: (string * Parsetree.expression) list;
    string_default: string option;
    int_default: string option;
  }

type typ =
  | Arrow of typ list * typ
  | Unit of Location.t
  | Js
  | Name of string * typ list
  | Enum of enum_params

type expr =
  | Id of string
  | Str of string
  | Call of expr * expr list

type valdef =
  | Cast
  | PropGet of string
  | PropSet of string
  | MethCall of string
  | Global of string
  | Expr of expr
  | New of string

type decl =
  | Module of string * decl list
  | Type of rec_flag * Parsetree.type_declaration list
  | Val of string * typ * valdef * Location.t

let is_unit = function
  | Unit _ -> true
  | _ -> false

(** Parsing *)

let rec parse_expr e =
  match e.pexp_desc with
  | Pexp_ident {txt=Lident s} -> Id s
  | Pexp_constant (Const_string (s, _)) -> Str s
  | Pexp_apply (e, args) ->
      Call
        (parse_expr e,
         List.map
           (function
             | (Nolabel, e) -> parse_expr e
             | _ -> error e.pexp_loc Invalid_expression
           )
           args
        )
  | _ ->
     error e.pexp_loc Invalid_expression

let expr_of_stritem = function
  | {pstr_desc=Pstr_eval (e, _); _} -> e
  | p -> error p.pstr_loc Expression_expected

let expr_of_payload loc = function
  | PStr [x] -> expr_of_stritem x
  | _ -> error loc Expression_expected

let typ_of_constant_exp x =
  match x.pexp_desc with
  | Pexp_constant (Const_string _) -> "string"
  | Pexp_constant (Const_int _) -> "int"
  | _ -> error x.pexp_loc Invalid_expression

let val_of_constant_exp x =
  match x.pexp_desc with
  | Pexp_constant c -> c
  | _ -> error x.pexp_loc Invalid_expression

let opt_cons x xs =
  match x with
  | None -> xs
  | Some x -> x :: xs

let prepare_enum label loc attributes =
  let js = ref {pexp_desc = Pexp_constant (Const_string (label, None)); pexp_loc = loc; pexp_attributes = attributes} in
  List.iter
    (fun (k, v) ->
      match k.txt with
      | "js" -> js := expr_of_payload k.loc v
      | _ -> ()
    )
    attributes;
  label, !js

let get_enums f l =
  let f (enums, string_default, int_default) x =
    match f x with
    | `Enum enum -> enum :: enums, string_default, int_default
    | `Default (loc, label, "int") ->
        begin match int_default with
        | None -> enums, string_default, Some label
        | Some _ -> error loc Multiple_default_case_in_enum
        end
    | `Default (loc, label, "string") ->
        begin match string_default with
        | None -> enums, Some label, int_default
        | Some _ -> error loc Multiple_default_case_in_enum
        end
    | `Default _ -> assert false
  in
  let enums, string_default, int_default = List.fold_left f ([], None, None) l in
  { enums; string_default; int_default }

let rec parse_typ ty =
  match ty.ptyp_desc with
  | Ptyp_arrow (_, t1, t2) ->
      let t1 = parse_typ t1 in
      begin match parse_typ t2 with
      | Arrow (tl, tres) when t2.ptyp_attributes = [] -> Arrow (t1 :: tl, tres)
      | tres -> Arrow ([t1], tres)
      end
  | Ptyp_constr ({txt = lid}, tl) ->
      begin match String.concat "." (Longident.flatten lid), tl with
      | "unit", [] -> Unit ty.ptyp_loc
      | "Ojs.t", [] -> Js
      | s, tl -> Name (s, List.map parse_typ tl)
      end
  | Ptyp_variant (rows, Closed, None) ->
      let prepare_row = function
        | Rtag (label, attributes, true, []) ->
            `Enum (prepare_enum label ty.ptyp_loc attributes)
        | Rtag (label, attributes, false, [{ptyp_desc = Ptyp_constr ({txt = lid; loc}, []); ptyp_attributes = _; ptyp_loc = _}]) when has_attribute "js.default" attributes ->
            begin match String.concat "." (Longident.flatten lid) with
            | ("int" | "string") as ty -> `Default (loc, label, ty)
            | _ -> error loc Default_case_in_enum
            end
        | _ -> error ty.ptyp_loc Non_constant_constructor_in_enum
      in
      Enum (get_enums prepare_row rows)
  | _ ->
      error ty.ptyp_loc Cannot_parse_type

let check_prefix ~prefix s =
  let l = String.length prefix in
  if l <= String.length s && String.sub s 0 l = prefix
  then
    Some (String.sub s l (String.length s - l))
  else
    None

let has_prefix ~prefix s = check_prefix ~prefix s <> None

let drop_prefix ~prefix s =
  match check_prefix ~prefix s with
  | Some x -> x
  | None -> assert false


let check_suffix ~suffix s =
  let l = String.length suffix in
  if l <= String.length s && String.sub s (String.length s - l) l = suffix
  then
    Some (String.sub s 0 (String.length s - l))
  else
    None

let has_suffix ~suffix s = check_suffix ~suffix s <> None

let drop_suffix ~suffix s =
  match check_suffix ~suffix s with
  | Some x -> x
  | None -> assert false

let auto s = function
  | Arrow ([Name (t, [])], Js) when check_suffix ~suffix:"_to_js" s = Some t -> Cast
  | Arrow ([Js], Name (t, [])) when check_suffix ~suffix:"_of_js" s = Some t -> Cast
  | Arrow ([Name _], _) ->  PropGet s
  | Arrow ([Name _; _], Unit _) when has_prefix ~prefix:"set_" s -> PropSet (drop_prefix ~prefix:"set_" s)
  | Arrow (_, Name _) when has_prefix ~prefix:"new_" s -> New (drop_prefix ~prefix:"new_" s)
  | Arrow (Name _ :: _, _) -> MethCall s
  | _ -> Global s

let id_of_expr = function
  | {pexp_desc=Pexp_constant (Const_string (s, _)); _}
  | {pexp_desc=Pexp_ident {txt=Lident s;_}; _}
  | {pexp_desc=Pexp_construct ({txt=Lident s;_}, None); _} -> s
  | e -> error e.pexp_loc Identifier_expected

let parse_valdecl ~in_sig vd =
  let s = vd.pval_name.txt in
  let loc = vd.pval_loc in
  let ty = lazy (parse_typ vd.pval_type) in
  let attrs = vd.pval_attributes in

  let parse_attr defs (k, v) =
    let opt_name ?(prefix = "") () =
      match v with
      | PStr [] ->
          begin match check_prefix ~prefix s with
          | None -> error loc Setter_name
          | Some s -> s
          end
      | _ -> id_of_expr (expr_of_payload k.loc v)
    in
    match k.txt with
    | "js.cast" ->
        Cast :: defs
    | "js.expr" ->
        Expr (parse_expr (expr_of_payload k.loc v)) :: defs
    | "js.get" ->
        PropGet (opt_name ()) :: defs
    | "js.set" ->
        PropSet (opt_name ~prefix:"set_" ()) :: defs
    | "js.meth" ->
        MethCall (opt_name ()) :: defs
    | "js.global" ->
        Global (opt_name ()) :: defs
    | "js" ->
        auto s (Lazy.force ty) :: defs
    | "js.new" ->
        New (opt_name ~prefix:"new_" ()) :: defs
    | _ ->
        defs
  in
  let defs = List.fold_left parse_attr [] attrs in
  let r =
    match defs with
    | [x] -> x
    | [] when in_sig -> auto s (Lazy.force ty)
    | [] -> raise Exit
    | _ -> error loc Multiple_binding_declarations
  in
  Val (s, Lazy.force ty, r, loc)

let rec parse_sig_item s =
  match s.psig_desc with
  | Psig_value vd when vd.pval_prim = [] ->
      parse_valdecl ~in_sig:true vd
  | Psig_type (rec_flag, decls) ->
      Type (rec_flag, decls)
  | Psig_module {pmd_name; pmd_type = {pmty_desc = Pmty_signature si; _}; _} ->
      Module (pmd_name.txt, parse_sig si)
  | _ ->
      error s.psig_loc Cannot_parse_sigitem

and parse_sig s = List.map parse_sig_item s

(** Code generation *)

let var x = Exp.ident (mknoloc (Longident.parse x))
let str s = Exp.constant (Const_string (s, None))

let fun_ s e =
  match e.pexp_desc with
  | Pexp_apply (f, [Nolabel, {pexp_desc = Pexp_ident {txt = Lident x}}])
      when x = s -> f
  | _ ->
      Exp.fun_ Nolabel None (Pat.var (mknoloc s)) e

let fun_unit e =
  match e.pexp_desc with
  | Pexp_apply (f, [Nolabel, {pexp_desc = Pexp_construct ({txt = Lident "()"}, None)}]) ->
      f
  | _ ->
      Exp.fun_ Nolabel None (Pat.construct (mknoloc (Lident "()")) None) e

let func args body =
  match args with
  | [] -> fun_unit body
  | args -> List.fold_right (fun s rest -> fun_ s rest) args body

let app f args =
  let args =
    match args with
    | [] -> [Exp.construct (mknoloc (Lident "()")) None]
    | args -> args
  in
  Exp.apply f (List.map (fun e -> (Nolabel, e)) args)

let ojs s args = app (Exp.ident (mknoloc (Ldot (Lident "Ojs", s)))) args

let def s ty body =
  Str.value Nonrecursive [ Vb.mk (Pat.constraint_ (Pat.var (mknoloc s)) ty) body ]

let uid = ref 0

let fresh () =
  incr uid;
  Printf.sprintf "x%i" !uid

let mkfun f =
  let s = fresh () in
  fun_ s (f (var s))


let builtin_type = function
  | "int" | "string" | "bool" | "float"
  | "array" | "list" | "option" -> true
  | _ -> false

let let_exp_in exp f =
  let x = fresh () in
  let pat = Pat.var (mknoloc x) in
  Exp.let_ Nonrecursive [Vb.mk pat exp] (f (Exp.ident (mknoloc (Longident.Lident x))))

let assert_false = Exp.assert_ (Exp.construct (mknoloc (Longident.parse "false")) None)

let rec js2ml ty exp =
  match ty with
  | Js ->
      exp
  | Name (s, tl) ->
      let s = if builtin_type s then "Ojs." ^ s else s in
      let args = List.map js2ml_fun tl in
      app (Exp.ident (mknoloc (Longident.parse (s ^ "_of_js")))) (args @ [exp])
  | Arrow (ty_args, ty_res) ->
      let args = gen_args ty_args in
      let res =
        ojs (if is_unit ty_res then "apply_unit" else "apply")
          [
            exp;
            Exp.array (List.map snd args)
          ]
      in
      func (List.map fst args) (map_res res ty_res)
  | Unit loc ->
      error loc Unit_not_supported_here
  | Enum params -> js2ml_of_enum ~variant:true params exp

and js2ml_of_enum ~variant {enums; string_default; int_default} exp =
  let mkval =
    if variant then fun x arg -> Exp.variant x arg
    else fun x arg -> Exp.construct (mknoloc (Longident.Lident x)) arg
  in
  let to_ml exp =
    let gen_match typ enums default =
      let f otherwise (ml, _ty, js) =
        let pat = Pat.constant (val_of_constant_exp js) in
        let mlval = mkval ml None in
        Exp.case pat mlval :: otherwise
      in
      let otherwise =
        match default with
        | None -> Exp.case (Pat.any ()) assert_false
        | Some label ->
            let x = fresh () in
            Exp.case (Pat.var (mknoloc x)) (mkval label (Some (Exp.ident (mknoloc (Longident.Lident x)))))
      in
      let cases = List.fold_left f [otherwise] enums in
      Exp.match_ (js2ml (Name (typ, [])) exp) cases
    in
    let enums = List.map (function (ml, js) -> (ml, typ_of_constant_exp js, js)) enums in
    let int_enums, string_enums = List.partition (function (_, ty, _) -> ty = "int") enums in
    let int_cases = gen_match "int" int_enums int_default in
    let string_cases = gen_match "string" string_enums string_default in
    match int_enums, int_default, string_enums, string_default with
    | [], None, _, _ -> int_cases
    | _, _, [], None -> string_cases
    | _ ->
        let case_int = Exp.case (Pat.constant (Const_string ("number", None))) int_cases in
        let case_string = Exp.case (Pat.constant (Const_string ("string", None))) string_cases in
        let case_default = Exp.case (Pat.any ()) assert_false in
        Exp.match_ (Exp.apply (Exp.ident (mknoloc (Longident.parse "Ojs.type_of"))) [Nolabel, exp]) [case_int; case_string; case_default]
  in
  let_exp_in exp to_ml

and ml2js ty exp =
  match ty with
  | Js ->
      exp
  | Name (s, tl) ->
      let s = if builtin_type s then "Ojs." ^ s else s in
      let args = List.map ml2js_fun tl in
      app (Exp.ident (mknoloc (Longident.parse (s ^ "_to_js")))) (args @ [exp])
  | Arrow (ty_args, ty_res) ->
      let args = gen_args ~map:js2ml ty_args in
      let res = app exp (List.map snd args) in
      let f = func (List.map fst args) (map_res ~map:ml2js res ty_res) in
      ojs "fun_to_js" [f]
  | Unit loc ->
      error loc Unit_not_supported_here
  | Enum params -> ml2js_of_enum ~variant:true params exp

and ml2js_of_enum ~variant {enums; string_default; int_default} exp =
  let mkpat =
    if variant then fun x arg -> Pat.variant x arg
    else fun x arg -> Pat.construct (mknoloc (Longident.Lident x)) arg
  in
  let gen_default ty default =
    match default with
    | None -> None
    | Some label ->
        let x = fresh () in
        let pat = mkpat label (Some (Pat.var (mknoloc x))) in
        let ty = Name (ty, []) in
        Some (Exp.case pat (ml2js ty (Exp.ident (mknoloc (Longident.Lident x)))))
  in
  let f (ml, js) =
    let pat = mkpat ml None in
    let ty = Name (typ_of_constant_exp js, []) in
    Exp.case pat (ml2js ty js)
  in
  let cases = opt_cons (gen_default "string" string_default) [] in
  let cases = opt_cons (gen_default "int" int_default) cases in
  let cases = List.map f enums @ cases in
  Exp.match_ exp cases

and js2ml_fun ty = mkfun (js2ml ty)
and ml2js_fun ty = mkfun (ml2js ty)

and gen_args ?(name = fun _ -> fresh ()) ?(map = ml2js) = function
  | [Unit _] -> []
  | args ->
      let f i ty =
        let s = name i in
        s, map ty (var s)
      in
      List.mapi f args

and map_res ?(map=js2ml) res ty_res =
  match ty_res with
  | Unit _ -> res
  | _ -> map ty_res res

and gen_typ = function
  | Name (s, tyl) ->
      Typ.constr (mknoloc (Longident.parse s)) (List.map gen_typ tyl)
  | Js ->
      Typ.constr (mknoloc (Longident.parse "Ojs.t")) []
  | Unit _ ->
      Typ.constr (mknoloc (Lident "unit")) []
  | Arrow (tl, t2) ->
      List.fold_right
        (fun t1 t2 -> Typ.arrow Nolabel (gen_typ t1) t2)
        tl
        (gen_typ t2)
  | Enum {enums; string_default; int_default} ->
      let gen_default ty default =
        match default with
        | None -> None
        | Some label -> Some (Rtag (label, [], false, [Typ.constr (mknoloc (Longident.Lident ty)) []]))
      in
      let f (label, _) = Rtag (label, [], true, []) in
      let rows = opt_cons (gen_default "string" string_default) [] in
      let rows = opt_cons (gen_default "int" int_default) rows in
      let rows = List.map f enums @ rows in
      Typ.variant rows Closed None

let rec gen_decls si =
  List.concat (List.map gen_decl si)

and gen_funs_record lbls =
  let prepare_label l =
    let js = ref l.pld_name.txt in
    List.iter
      (fun (k, v) ->
         match k.txt with
         | "js" -> js := id_of_expr (expr_of_payload k.loc v)
         | _ -> ()
      )
      l.pld_attributes;

    mknoloc (Lident l.pld_name.txt), (* OCaml label *)
    str !js, (* JS name *)
    parse_typ l.pld_type
  in
  let lbls = List.map prepare_label lbls in
  let of_js x (ml, js, ty) =
    ml, js2ml ty (ojs "get" [x; js])
  in
  let to_js x (ml, js, ty) =
    Exp.tuple [js; ml2js ty (Exp.field x ml)]
  in

  mkfun (fun x -> Exp.record (List.map (of_js x) lbls) None),
  mkfun (fun x -> ojs "obj" [Exp.array (List.map (to_js x) lbls)])

and gen_funs_enums constructors =
  let prepare_constructor c =
    begin match c.pcd_res, c.pcd_args with
    | None, Pcstr_tuple [] ->
        `Enum (prepare_enum c.pcd_name.txt c.pcd_name.loc c.pcd_attributes)
    | None, Pcstr_tuple [{ptyp_desc = Ptyp_constr ({txt = lid; loc}, []); ptyp_attributes = _; ptyp_loc = _}] when has_attribute "js.default" c.pcd_attributes ->
        begin match String.concat "." (Longident.flatten lid) with
        | ("int" | "string") as ty -> `Default (c.pcd_loc, c.pcd_name.txt, ty)
        | _ -> error loc Default_case_in_enum
        end
    | _ ->
        error c.pcd_loc Non_constant_constructor_in_enum
    end
  in
  let params = get_enums prepare_constructor constructors in
  mkfun (js2ml_of_enum ~variant:false params),
  mkfun (ml2js_of_enum ~variant:false params)

and gen_funs p =
  let name = p.ptype_name.txt in
  let of_js, to_js =
    match p.ptype_manifest, p.ptype_kind with
    | Some ty, Ptype_abstract ->
        let ty = parse_typ ty in
        js2ml_fun ty, ml2js_fun ty
    | _, Ptype_variant cstrs ->
        gen_funs_enums cstrs
    | _, Ptype_record lbls ->
        gen_funs_record lbls
    | _ ->
        error p.ptype_loc Cannot_parse_type
  in
  [
    Vb.mk
      ~loc:p.ptype_loc
      (Pat.constraint_
         (Pat.var (mknoloc (name ^ "_of_js")))
         (gen_typ (Arrow ([Js], Name (name, [])))))
      of_js;
    Vb.mk
      ~loc:p.ptype_loc
      (Pat.constraint_
         (Pat.var (mknoloc (name ^ "_to_js")))
         (gen_typ (Arrow ([Name (name, [])], Js))))
      to_js
  ]

and gen_decl = function
  | Type (rec_flag, decls) ->
      let decls = List.map (fun t -> {t with ptype_private = Public}) decls in
      let funs = List.concat (List.map gen_funs decls) in
      [ Str.type_ rec_flag decls; Str.value rec_flag funs ]
  | Module (s, decls) ->
      [ Str.module_ (Mb.mk (mknoloc s) (Mod.structure (gen_decls decls))) ]

  | Val (s, ty, decl, loc) ->
      let d = gen_def loc decl ty in
      [ def s (gen_typ ty) d ]

and gen_def loc decl ty =
  match decl, ty with
  | Cast, Arrow ([ty_arg], ty_res) ->
      mkfun (fun this -> js2ml ty_res (ml2js ty_arg this))

  | PropGet s, Arrow ([ty_this], ty_res) ->
      mkfun (fun this -> js2ml ty_res (ojs "get" [ml2js ty_this this; str s]))

  | PropSet s, Arrow ([Name _ as ty_this; ty_arg], Unit _) ->
      let res this arg =
        ojs "set"
          [
            ml2js ty_this this;
            str s;
            ml2js ty_arg arg
          ]
      in
      mkfun (fun this -> mkfun (fun arg -> res this arg))

  | MethCall s, Arrow (ty_this :: ty_args, ty_res) ->
      let args = gen_args ty_args in
      let res this =
        ojs
          (if is_unit ty_res then "call_unit" else "call")
          [
            ml2js ty_this this;
            str s;
            Exp.array (List.map snd args)
          ]
      in
      mkfun
        (fun this ->
           func (List.map fst args) (map_res (res this) ty_res)
        )

  | Global s, ty_res ->
      let res = ojs "variable" [str s] in
      js2ml ty_res res

  | Expr e, Arrow (ty_args, ty_res) ->
      let args = gen_args ~name:(Printf.sprintf "arg%i") ty_args in
      func (List.map fst args) (map_res (gen_expr loc args e) ty_res)

  | Expr e, ty ->
      js2ml ty (gen_expr loc [] e)

  | New name, Arrow (ty_args, ty_res) ->
      let args = gen_args ty_args in
      let res = ojs "new_obj" [str name; Exp.array (List.map snd args)] in
      func (List.map fst args) (map_res res ty_res)

  | _ ->
      error loc Binding_type_mismatch

and gen_expr loc ids = function
  | Call (Id "call", obj :: Str meth :: args) ->
      ojs "call"
        [
          gen_expr loc ids obj;
          str meth;
          Exp.array (List.map (gen_expr loc ids) args)
        ]
  | Call (Id "global", [Str s]) ->
      ojs "variable" [str s]
  | Str s ->
      ml2js (Name ("string", [])) (str s)
  | Id s when List.mem_assoc s ids ->
      List.assoc s ids
  | _ ->
      error loc Invalid_expression


let attr s e = Str.attribute (mknoloc s, PStr [Str.eval e])

let disable_warnings = attr "ocaml.warning" (str "-32-39")
    (* 32: unused value declarations (when *_of_js, *_to_js are not needed)
       39: unused rec flag (for *_of_js, *_to_js functions, when the
           type is not actually recursive) *)

let str_of_sg sg =
  let decls = parse_sig sg in
  attr "comment" (str "!! This code has been generated by gen_js_api !!") ::
  disable_warnings ::
  gen_decls decls

(** ppx mapper *)

let incl = function
  | [x] -> x
  | str -> Str.include_ (Incl.mk (Mod.structure str))

let mapper =
  let open Ast_mapper in
  let super = default_mapper in
  let module_expr self mexp =
    let mexp = super.module_expr self mexp in
    match mexp.pmod_desc with
    | Pmod_constraint({pmod_desc=Pmod_extension ({txt="js"}, PStr[])},
                      ({pmty_desc=Pmty_signature sg} as mty)) ->
        Mod.constraint_ (Mod.structure (str_of_sg sg))
          mty
    | _ -> mexp
  in
  let structure_item self str =
    let str = super.structure_item self str in
    match str.pstr_desc with
    | Pstr_primitive vd when vd.pval_prim = [] ->
        begin match parse_valdecl ~in_sig:false vd with
        | exception Exit -> str
        | d -> incl (gen_decls [d])
        end
    | Pstr_type (rec_flag, decls) ->
        let js_decls =
          List.filter
            (fun d ->
               List.exists (fun (k, _) -> k.txt = "js") d.ptype_attributes
            ) decls
        in
        begin match js_decls with
        | [] -> str
        | l ->
            with_default_loc str.pstr_loc
              (fun () ->
                 let funs = List.concat (List.map gen_funs l) in
                 incl
                   [
                     str;
                     disable_warnings;
                     Str.value ~loc:str.pstr_loc rec_flag funs
                   ]
              )
        end

    | _ ->
        str
  in
  let expr self e =
    let e = super.expr self e in
    match e.pexp_desc with
    | Pexp_extension ({txt="js.to"}, PTyp ty) -> js2ml_fun (parse_typ ty)
    | Pexp_extension ({txt="js.of"}, PTyp ty) -> ml2js_fun (parse_typ ty)
    | _ ->
        e
  in
  {super with module_expr; structure_item; expr}


(** Main *)


let standalone () =
  let sg =
    Pparse.parse_interface Format.err_formatter
      ~tool_name:"gen_js_iface"
      Sys.argv.(1)
  in
  let res = str_of_sg sg in
  Format.printf "%a@." Pprintast.structure res


let () =
  try
    if Array.length Sys.argv < 4 || Sys.argv.(1) <> "-ppx" then standalone ()
    else Ast_mapper.run_main (fun _ -> mapper)
  with exn ->
    Format.eprintf "%a@." Location.report_exception exn;
    exit 2
