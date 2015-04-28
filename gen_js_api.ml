(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

open Location
open Asttypes
open Parsetree
open Longident
open Ast_helper

(** Errors *)

type error =
  | Expression_expected
  | Identifier_expected
  | Structure_expected
  | Invalid_expression
  | Multiple_binding_declarations
  | Binding_type_mismatch
  | Cannot_parse_type
  | Cannot_parse_sigitem
  | Cannot_parse_classdecl
  | Cannot_parse_classfield
  | Implicit_name of string
  | Unit_not_supported_here
  | Non_constant_constructor_in_enum
  | Default_case_in_enum
  | Multiple_default_case_in_enum
  | Invalid_variadic_type_arg
  | No_input
  | Multiple_inputs
  | Unlabelled_argument_in_builder
  | Spurious_attribute
  | Union_not_supported_here

exception Error of Location.t * error

let used_attributes_tbl = Hashtbl.create 16
let register_loc loc = Hashtbl.replace used_attributes_tbl loc ()
let is_registered_loc loc = Hashtbl.mem used_attributes_tbl loc

let error loc err = raise (Error (loc, err))

let filter_attr key = function ({txt; loc}, _) ->
  if txt = key then begin
    register_loc loc;
    true
  end else false

let has_attribute key attrs = List.exists (filter_attr key) attrs

let get_attribute key attrs =
  match List.find (filter_attr key) attrs with
  | exception Not_found -> None
  | (k, v) -> Some (k, v)

let unoption = function
  | Some x -> x
  | None -> assert false

let expr_of_stritem = function
  | {pstr_desc=Pstr_eval (e, _); _} -> e
  | p -> error p.pstr_loc Expression_expected

let expr_of_payload loc = function
  | PStr [x] -> expr_of_stritem x
  | _ -> error loc Expression_expected

let str_of_payload loc = function
  | PStr x -> x
  | _ -> error loc Structure_expected

let id_of_expr = function
  | {pexp_desc=Pexp_constant (Const_string (s, _)); _}
  | {pexp_desc=Pexp_ident {txt=Lident s;_}; _}
  | {pexp_desc=Pexp_construct ({txt=Lident s;_}, None); _} -> s
  | e -> error e.pexp_loc Identifier_expected

let get_expr_attribute key attrs =
  match get_attribute key attrs with
  | None -> None
  | Some (k, v) -> Some (expr_of_payload k.loc v)

let get_string_attribute key attrs =
  match get_attribute key attrs with
  | None -> None
  | Some (k, v) -> Some (id_of_expr (expr_of_payload k.loc v))

let print_error ppf = function
  | Expression_expected ->
      Format.fprintf ppf "Expression expected"
  | Structure_expected ->
      Format.fprintf ppf "Structure expected"
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
  | Cannot_parse_classdecl ->
      Format.fprintf ppf "Cannot parse class declaration"
  | Cannot_parse_classfield ->
      Format.fprintf ppf "Cannot parse class field"
  | Implicit_name prefix ->
      Format.fprintf ppf "Implicit name must start with '%s'" prefix
  | Unit_not_supported_here ->
      Format.fprintf ppf "Unit not supported in this context"
  | Non_constant_constructor_in_enum ->
      Format.fprintf ppf "Constructors in enums cannot take arguments"
  | Default_case_in_enum ->
      Format.fprintf ppf "Default constructor in enums unique argument must be of type int or string"
  | Multiple_default_case_in_enum ->
      Format.fprintf ppf "At most one default constructor is supported in enums for each type"
  | Invalid_variadic_type_arg ->
      Format.fprintf ppf "A variadic function argument must be of type list"
  | No_input ->
      Format.fprintf ppf "An input file must be provided"
  | Multiple_inputs ->
      Format.fprintf ppf "A single input file must be provided"
  | Unlabelled_argument_in_builder ->
      Format.fprintf ppf "Arguments of builder must be named"
  | Spurious_attribute ->
      Format.fprintf ppf "Spurious js.* attribute"
  | Union_not_supported_here ->
      Format.fprintf ppf "js.union not supported in this context"

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) -> Some (Location.error_of_printer loc print_error err)
      | _ -> None
    )

let js_name ?(capitalize = false) name =
  let n = String.length name in
  let buf = Buffer.create n in
  let capitalize = ref capitalize in
  for i = 0 to n-1 do
    let c = name.[i] in
    if c = '_' then capitalize := true
    else if !capitalize then begin
      Buffer.add_char buf (Char.uppercase_ascii c);
      capitalize := false
    end else Buffer.add_char buf c
  done;
  Buffer.contents buf

(** AST *)

type enum_params =
  {
    enums: (string * Parsetree.expression) list;
    string_default: string option;
    int_default: string option;
  }

type typ =
  | Arrow of arrow_params
  | Unit of Location.t
  | Js
  | Name of string * typ list
  | Enum of enum_params
  | Union of Location.t * (string * typ option) list
  | Tuple of typ list

and lab =
  | Arg
  | Lab of {ml: string}
  | Opt of {ml: string; def: Parsetree.expression option}

and arg =
  {
    lab: lab;
    att: attributes;
    typ: typ;
  }

and arrow_params =
  {
    ty_args: arg list;
    ty_vararg: arg option;
    unit_arg: bool;
    ty_res: typ;
  }

let arg_label = function
  | Arg -> Nolabel
  | Lab {ml; _} -> Labelled ml
  | Opt {ml; _} -> Optional ml

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
  | Builder

type methoddef =
  | Getter of string
  | Setter of string
  | MethodCall of string

type method_decl =
  {
    method_name: string;
    method_typ: typ;
    method_def: methoddef;
    method_loc: Location.t;
  }

type class_field =
  | Method of method_decl
  | Inherit of Longident.t Location.loc

type classdecl =
  | Declaration of { class_name: string; class_fields: class_field list }
  | Constructor of { class_name: string; js_class_name: string; class_arrow: arrow_params }

type decl =
  | Module of string * decl list
  | Type of rec_flag * Parsetree.type_declaration list
  | Val of string * typ * valdef * Location.t
  | Class of classdecl list
  | Implem of Parsetree.structure

(** Parsing *)

let rec parse_expr e =
  match e.pexp_desc with
  | Pexp_ident {txt=Lident s; loc = _} -> Id s
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
      | "js" -> register_loc k.loc; js := expr_of_payload k.loc v
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

let rec parse_arg lab ty =
  let lab =
    match lab with
    | Nolabel -> Arg
    | Labelled ml -> Lab {ml}
    | Optional ml ->
        Opt {ml; def=get_expr_attribute "js.default" ty.ptyp_attributes}
  in
  {
    lab;
    att=ty.ptyp_attributes;
    typ = parse_typ ty;
  }

and parse_typ ty =
  match ty.ptyp_desc with
  | Ptyp_arrow (lab, t1, t2) when has_attribute "js.variadic" t1.ptyp_attributes ->
      begin match parse_arg lab t1 with
      | {lab; att; typ=Name ("list", [typ])} ->
          let ty_vararg = Some {lab; att; typ} in
          begin match parse_typ t2 with
          | Arrow ({ty_args = []; ty_vararg = None; unit_arg = _; ty_res = _} as params) when t2.ptyp_attributes = [] ->
              Arrow {params with ty_vararg}
          | Arrow _ when t2.ptyp_attributes = [] -> error ty.ptyp_loc Cannot_parse_type
          | tres -> Arrow {ty_args = []; ty_vararg; unit_arg = false; ty_res = tres}
          end
      | _ -> error t1.ptyp_loc Invalid_variadic_type_arg
      end
  | Ptyp_arrow (lab, t1, t2) ->
      let t1 = parse_arg lab t1 in
      begin match parse_typ t2 with
      | Arrow ({ty_args; ty_vararg = _; unit_arg = _; ty_res = _} as params) when t2.ptyp_attributes = [] -> Arrow {params with ty_args = t1 :: ty_args}
      | tres ->
          begin match t1 with
          | {lab=Arg; att=[]; typ=Unit _} -> Arrow {ty_args = []; ty_vararg = None; unit_arg = true; ty_res = tres}
          | _ -> Arrow {ty_args = [t1]; ty_vararg = None; unit_arg = false; ty_res = tres}
          end
      end
  | Ptyp_constr ({txt = lid; loc = _}, tl) ->
      begin match String.concat "." (Longident.flatten lid), tl with
      | "unit", [] -> Unit ty.ptyp_loc
      | "Ojs.t", [] -> Js
      | s, tl -> Name (s, List.map parse_typ tl)
      end
  | Ptyp_variant (rows, Closed, None) when has_attribute "js.union" ty.ptyp_attributes ->
      let prepare_row = function
        | Rtag (label, _attributes, true, []) -> (label, None)
        | Rtag (label, _attributes, false, [typ]) -> (label, Some (parse_typ typ))
        | _ -> error ty.ptyp_loc Cannot_parse_type
      in
      Union (ty.ptyp_loc, List.map prepare_row rows)
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
  | Ptyp_tuple typs ->
      let typs = List.map parse_typ typs in
      Tuple typs
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

let auto s = function
  | Arrow {ty_args = [{lab=Arg; att=_; typ=Name (t, [])}]; ty_vararg = None; unit_arg = false; ty_res = Js} when check_suffix ~suffix:"_to_js" s = Some t -> Cast
  | Arrow {ty_args = [{lab=Arg; att=_; typ=Js}]; ty_vararg = None; unit_arg = false; ty_res = Name (t, [])} when check_suffix ~suffix:"_of_js" s = Some t -> Cast
  | Arrow {ty_args = [{lab=Arg; att=_; typ=Name _}]; ty_vararg = None; unit_arg = false; ty_res = _} ->  PropGet (js_name s)
  | Arrow {ty_args = [{lab=Arg; att=_; typ=Name _}; _]; ty_vararg = None; unit_arg = false; ty_res = Unit _} when has_prefix ~prefix:"set_" s -> PropSet (js_name (drop_prefix ~prefix:"set_" s))
  | Arrow {ty_args = _; ty_vararg = None; unit_arg = false; ty_res = Name _} when has_prefix ~prefix:"new_" s -> New (js_name (drop_prefix ~prefix:"new_" s))
  | Arrow {ty_args = {lab=Arg; att=_; typ=Name _} :: _; ty_vararg = _; unit_arg = _; ty_res = _} -> MethCall (js_name s)
  | _ -> Global (js_name s)

let auto_in_object s = function
  | Arrow {ty_args = [{lab=Arg; att=_; typ=_}]; ty_vararg = None; unit_arg = false; ty_res = Unit _} when has_prefix ~prefix:"set_" s -> PropSet (js_name (drop_prefix ~prefix:"set_" s))
  | Arrow _ -> MethCall (js_name s)
  | _ -> PropGet (js_name s)

let parse_attr (s, loc, auto) defs (k, v) =
  let opt_name ?(prefix = "") ?(capitalize = false) () =
    match v with
    | PStr [] ->
        begin match check_prefix ~prefix s with
        | None -> error loc (Implicit_name prefix)
        | Some s -> js_name ~capitalize s
        end
    | _ -> id_of_expr (expr_of_payload k.loc v)
  in
  match k.txt with
  | "js.cast" ->
      register_loc k.loc;
      Cast :: defs
  | "js.expr" ->
      register_loc k.loc;
      Expr (parse_expr (expr_of_payload k.loc v)) :: defs
  | "js.get" ->
      register_loc k.loc;
      PropGet (opt_name ()) :: defs
  | "js.set" ->
      register_loc k.loc;
      PropSet (opt_name ~prefix:"set_" ()) :: defs
  | "js.call" ->
      register_loc k.loc;
      MethCall (opt_name ()) :: defs
  | "js.global" ->
      register_loc k.loc;
      Global (opt_name ()) :: defs
  | "js" ->
      register_loc k.loc;
      auto () :: defs
  | "js.new" ->
      register_loc k.loc;
      New (opt_name ~prefix:"new_" ~capitalize:true ()) :: defs
  | "js.builder" ->
      register_loc k.loc;
      Builder :: defs
  | _ ->
      defs

let parse_valdecl ~in_sig vd =
  let s = vd.pval_name.txt in
  let loc = vd.pval_loc in
  let ty = parse_typ vd.pval_type in
  let attrs = vd.pval_attributes in
  let auto () = auto s ty in
  let defs = List.fold_left (parse_attr (s, loc, auto)) [] attrs in
  let r =
    match defs with
    | [x] -> x
    | [] when in_sig -> auto ()
    | [] -> raise Exit
    | _ -> error loc Multiple_binding_declarations
  in
  Val (s, ty, r, loc)

let rec parse_sig_item s =
  match s.psig_desc with
  | Psig_value vd when vd.pval_prim = [] ->
      parse_valdecl ~in_sig:true vd
  | Psig_type (rec_flag, decls) ->
      Type (rec_flag, decls)
  | Psig_module {pmd_name; pmd_type = {pmty_desc = Pmty_signature si; _}; _} ->
      Module (pmd_name.txt, parse_sig si)
  | Psig_class cs -> Class (List.map parse_class_decl cs)
  | Psig_attribute ({txt="js.implem"; loc}, PStr str) ->
      register_loc loc;
      Implem str
  | _ ->
      error s.psig_loc Cannot_parse_sigitem

and parse_sig = function
  | [] -> []
  | {psig_desc = Psig_attribute ({txt="js.stop"; _}, _); _} :: rest ->
      parse_sig_verbatim rest
  | {psig_desc = Psig_value vd; _} :: rest when
      has_attribute "js.custom" vd.pval_attributes ->
      let (k, v) = unoption (get_attribute "js.custom" vd.pval_attributes) in
      let str = str_of_payload k.loc v in
      Implem str :: parse_sig rest
  | s :: rest -> parse_sig_item s :: parse_sig rest

and parse_sig_verbatim = function
  | [] -> []
  | {psig_desc = Psig_attribute ({txt="js.start"; loc}, _); _} :: rest ->
      register_loc loc;
      parse_sig rest
  | _ :: rest -> parse_sig_verbatim rest

and parse_class_decl = function
  | {pci_virt = Concrete; pci_params = []; pci_name; pci_expr = {pcty_desc = Pcty_arrow (Nolabel, {ptyp_desc = Ptyp_constr ({txt = Longident.Ldot (Lident "Ojs", "t"); loc = _}, []); _}, {pcty_desc = Pcty_signature {pcsig_self = {ptyp_desc = Ptyp_any; _}; pcsig_fields}; _}); _}; _} ->
      let class_name = pci_name.txt in
      Declaration { class_name; class_fields = List.map parse_class_field pcsig_fields }
  | {pci_virt = Concrete; pci_params = []; pci_name; pci_expr; pci_attributes; pci_loc} ->
      let rec convert_typ = function
        | { pcty_desc = Pcty_constr (id, typs); pcty_attributes; pcty_loc } ->
            Typ.constr ~loc:pcty_loc ~attrs:pcty_attributes id typs
        | { pcty_desc = Pcty_arrow (label, typ, ct); pcty_attributes; pcty_loc } ->
            Typ.arrow ~loc:pcty_loc ~attrs:pcty_attributes label typ (convert_typ ct)
        | _ -> error pci_loc Cannot_parse_classdecl
      in
      let class_arrow =
        match parse_typ (convert_typ pci_expr) with
        | Arrow ({ty_args = _; ty_vararg = _; unit_arg = _; ty_res = Name (_, [])} as params) -> params
        | (Name (_, []) as ty_res) -> {ty_args = []; ty_vararg = None; unit_arg = false; ty_res}
        | _ -> error pci_loc Cannot_parse_classdecl
      in
      let class_name = pci_name.txt in
      let js_class_name =
        match get_string_attribute "js.new" pci_attributes with
        | None -> js_name ~capitalize:true class_name
        | Some s -> s
      in
      Constructor {class_name; js_class_name; class_arrow}
  | {pci_loc; _} -> error pci_loc Cannot_parse_classdecl

and parse_class_field = function
  | {pctf_desc = Pctf_method (method_name, Public, Concrete, typ); pctf_loc; pctf_attributes} ->
      let ty = parse_typ typ in
      let auto () = auto_in_object method_name ty in
      let defs = List.fold_left (parse_attr (method_name, pctf_loc, auto)) [] pctf_attributes in
      let kind =
        match defs with
        | [x] -> x
        | [] -> auto ()
        | _ -> error pctf_loc Multiple_binding_declarations
      in
      let method_def =
        match kind with
        | PropGet s -> Getter s
        | PropSet s -> Setter s
        | MethCall s -> MethodCall s
        | _ -> error pctf_loc Cannot_parse_classfield
      in
      Method
        {
          method_name;
          method_typ = ty;
          method_def;
          method_loc = pctf_loc;
        }
  | {pctf_desc = Pctf_inherit {pcty_desc = Pcty_constr (id, []); _}; _} ->
      Inherit id
  | {pctf_loc; _} -> error pctf_loc Cannot_parse_classfield

(** Code generation *)

let var x = Exp.ident (mknoloc (Longident.parse x))
let str s = Exp.constant (Const_string (s, None))
let int n = Exp.constant (Const_int n)

let attr s e = Str.attribute (mknoloc s, PStr [Str.eval e])

let disable_warnings = attr "ocaml.warning" (str "-7-32-39")
    (*  7: method overridden.
       32: unused value declarations (when *_of_js, *_to_js are not needed)
       39: unused rec flag (for *_of_js, *_to_js functions, when the
           type is not actually recursive) *)


let incl = function
  | [x] -> x
  | str -> Str.include_ (Incl.mk (Mod.structure str))

let nolabel args = List.map (function x -> Nolabel, x) args

let ojs_typ = Typ.constr (mknoloc (Longident.parse "Ojs.t")) []
let ojs s args = Exp.apply (Exp.ident (mknoloc (Ldot (Lident "Ojs", s)))) (nolabel args)

let list_map f x =
  Exp.apply (Exp.ident (mknoloc (Longident.parse "List.map"))) (nolabel [f; x])

let array_of_list x =
  Exp.apply (Exp.ident (mknoloc (Longident.parse "Array.of_list"))) (nolabel [x])

let array_append a1 a2 =
  (* perhaps use a specialized runtime primitive which returns
     one of the arrays if the other one is empty *)
  match a1.pexp_desc with
  | Pexp_array [] -> a2
  | _ -> ojs "caml_array_append" [a1; a2]

let fun_ (label, s) e =
  match e.pexp_desc with
  | Pexp_apply (f, [Nolabel, {pexp_desc = Pexp_ident {txt = Lident x; loc = _}; _}])
      when x = s -> f
  | _ ->
      Exp.fun_ label None (Pat.var (mknoloc s)) e

let fun_unit e =
  match e.pexp_desc with
  | Pexp_apply (f, [Nolabel, {pexp_desc = Pexp_construct ({txt = Lident "()"; loc = _}, None); _}]) ->
      f
  | _ ->
      Exp.fun_ Nolabel None (Pat.construct (mknoloc (Lident "()")) None) e

let func args unit_arg body =
  let body = if unit_arg then fun_unit body else body in
  List.fold_right (fun s rest -> fun_ s rest) args body

let uid = ref 0

let fresh () =
  incr uid;
  Printf.sprintf "x%i" !uid

let mkfun f =
  let s = fresh () in
  fun_ (Nolabel, s) (f (var s))

let apply f args = Exp.apply f args

let unit_lid = mknoloc (Lident "()")
let unit_expr = Exp.construct unit_lid None
let unit_pat = Pat.construct unit_lid None

let some_pat arg =
  Pat.construct (mknoloc (Longident.parse "Some")) (Some arg)

let none_pat () =
  Pat.construct (mknoloc (Longident.parse "None")) None

let match_some_none ~some ~none exp =
  let s = fresh () in
  Exp.match_ exp
    [
      Exp.case (some_pat (Pat.var (mknoloc s))) (some (var s));
      Exp.case (none_pat ()) none;
    ]

let app f args unit_arg =
  let args = if unit_arg then args @ [Nolabel, unit_expr] else args in
  apply f args

let split sep s =
  let n = String.length s in
  let rec aux start i =
    if i < n then
      if s.[i] = sep then String.sub s start (i - start) :: aux (i+1) (i+1)
      else aux start (i+1)
    else [String.sub s start (i - start)]
  in
  aux 0 0

let ojs_variable s =
  let path = split '.' s in
  match path with
  | [] -> assert false
  | x :: xs -> List.fold_left (fun o x -> ojs "get" [o; str x]) (ojs "variable" [str x]) xs

let def s ty body =
  Str.value Nonrecursive [ Vb.mk (Pat.constraint_ (Pat.var (mknoloc s)) ty) body ]



let builtin_type = function
  | "int" | "string" | "bool" | "float"
  | "array" | "list" | "option" -> true
  | _ -> false

let let_exp_in exp f =
  let x = fresh () in
  let pat = Pat.var (mknoloc x) in
  Exp.let_ Nonrecursive [Vb.mk pat exp] (f (var x))

let assert_false = Exp.assert_ (Exp.construct (mknoloc (Longident.parse "false")) None)

let rec js2ml ty exp =
  match ty with
  | Js ->
      exp
  | Name (s, tl) ->
      let s = if builtin_type s then "Ojs." ^ s else s in
      let args = List.map js2ml_fun tl in
      app (var (s ^ "_of_js")) (nolabel (args @ [exp])) false
  | Arrow {ty_args; ty_vararg; unit_arg; ty_res} ->
      let formal_args, concrete_args = add_variadic_arg (gen_args ty_args) ty_vararg in
      let res = ojs "apply" [exp; concrete_args] in
      func formal_args unit_arg (js2ml_unit ty_res res)
  | Unit loc ->
      error loc Unit_not_supported_here
  | Enum params -> js2ml_of_enum ~variant:true params exp
  | Union (loc, _) -> error loc Union_not_supported_here
  | Tuple typs ->
      let f x =
        Exp.tuple (List.mapi (fun i typ -> js2ml typ (ojs "array_get" [x; int i])) typs)
      in
      let_exp_in exp f

and js2ml_of_enum ~variant {enums; string_default; int_default} exp =
  let mkval =
    if variant then fun x arg -> Exp.variant x arg
    else fun x arg -> Exp.construct (mknoloc (Longident.Lident x)) arg
  in
  let to_ml exp =
    let gen_cases enums default =
      let f otherwise (ml, js) =
        let pat = Pat.constant (val_of_constant_exp js) in
        let mlval = mkval ml None in
        Exp.case pat mlval :: otherwise
      in
      match enums, default with
      | [], None -> []
      | _ ->
          let otherwise =
            match default with
            | None -> Exp.case (Pat.any ()) assert_false
            | Some label ->
                let x = fresh () in
                Exp.case (Pat.var (mknoloc x)) (mkval label (Some (var x)))
          in
          List.fold_left f [otherwise] enums
    in
    let mk_match typ cases = Exp.match_ (js2ml (Name (typ, [])) exp) cases in
    let int_enums, string_enums = List.partition (function (_, js) -> typ_of_constant_exp js = "int") enums in
    let int_cases = gen_cases int_enums int_default in
    let string_cases = gen_cases string_enums string_default in
    match int_cases, string_cases with
    | [], cases -> mk_match "string" cases
    | cases, [] -> mk_match "int" cases
    | _ ->
        let case_int = Exp.case (Pat.constant (Const_string ("number", None))) (mk_match "int" int_cases) in
        let case_string = Exp.case (Pat.constant (Const_string ("string", None))) (mk_match "string" string_cases) in
        let case_default = Exp.case (Pat.any ()) assert_false in
        Exp.match_ (ojs "type_of" [exp]) [case_int; case_string; case_default]
  in
  let_exp_in exp to_ml

and ml2js ty exp =
  match ty with
  | Js ->
      exp
  | Name (s, tl) ->
      let s = if builtin_type s then "Ojs." ^ s else s in
      let args = List.map ml2js_fun tl in
      app (var (s ^ "_to_js")) (nolabel (args @ [exp])) false
  | Arrow {ty_args; ty_vararg = None; unit_arg; ty_res} ->
      let args =
        let f _i {lab; att=_; typ} =
          let s = fresh() in
          let typ =
            match lab with
            | Arg | Lab _ -> typ
            | Opt _ -> Name ("Ojs.optdef", [typ])
          in
          s, (arg_label lab, js2ml typ (var s))
        in
        List.mapi f ty_args
      in
      let formal_args, concrete_args = List.map fst args, List.map snd args in
      let res = ml2js_unit ty_res (app exp concrete_args unit_arg) in
      let body = if formal_args = [] then Exp.fun_ Nolabel None (Pat.any ()) res else res in
      let f = List.fold_right (fun s -> fun_ (Nolabel, s)) formal_args body in
      ojs "fun_to_js" [int (max 1 (List.length formal_args)); f]
  | Arrow {ty_args; ty_vararg = Some {lab=label_variadic; att=_; typ=ty_variadic};
           unit_arg; ty_res} ->
      let arguments = fresh() in
      let n_args = List.length ty_args in
      let concrete_args =
        List.mapi
          (fun i {lab; att=_; typ} -> arg_label lab, js2ml typ (ojs "array_get" [var arguments; int i])) ty_args
      in
      let extra_arg = ojs "list_of_js_from" [ js2ml_fun ty_variadic; var arguments; int n_args ] in
      let extra_arg =
        match label_variadic with
        | Arg | Lab _ -> extra_arg
        | Opt _ -> Exp.construct (mknoloc (Longident.parse "Some")) (Some extra_arg)
      in
      let concrete_args = concrete_args @ [arg_label label_variadic, extra_arg] in
      let res = app exp concrete_args unit_arg in
      let f = func [Nolabel, arguments] false (ml2js_unit ty_res res) in
      ojs "fun_to_js_args" [f]
  | Unit loc ->
      error loc Unit_not_supported_here
  | Enum params -> ml2js_of_enum ~variant:true params exp
  | Union (_, cases) ->
      let f (label, params) =
        match params with
        | None ->
            let pat = Pat.variant label None in
            Exp.case pat (ojs "null" [])
        | Some ty ->
            let x = fresh () in
            let pat = Pat.variant label (Some (Pat.var (mknoloc x))) in
            Exp.case pat (ml2js ty (var x))
      in
      let cases = List.map f cases in
      Exp.match_ (Exp.constraint_ exp (gen_typ ty)) cases
  | Tuple typs ->
      let typed_vars = List.mapi (fun i typ -> i, typ, fresh ()) typs in
      let pat = Pat.tuple (List.map (function (_, _, x) -> Pat.var (mknoloc x)) typed_vars) in
      Exp.let_ Nonrecursive [Vb.mk pat exp] begin
        let n = List.length typs in
        let a = fresh () in
        let new_array = ojs "array_make" [int n] in
        Exp.let_ Nonrecursive [Vb.mk (Pat.var (mknoloc a)) new_array] begin
          let f e (i, typ, x) =
            Exp.sequence (ojs "array_set" [var a; int i; ml2js typ (var x)]) e
          in
          List.fold_left f (var a) (List.rev typed_vars)
        end
      end

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
        Some (Exp.case pat (ml2js ty (var x)))
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

and gen_args ?(name = fun _ -> fresh ()) args =
  let f i {lab; att=_; typ} =
    let s = name i in
    let e =
      match lab with
      | Arg | Lab _ -> ml2js typ (var s)
      | Opt {def; _} ->
          begin match def with
          | None -> ml2js (Name ("Ojs.optdef", [typ])) (var s)
          | Some none ->
              ml2js typ (match_some_none ~none ~some:(fun v -> v) (var s))
          end
    in
    (arg_label lab, s), (lab, e)
  in
  List.mapi f args

and add_variadic_arg args ty_variadic =
  let formal_args = List.map fst args in
  let concrete_args =  Exp.array (List.map (fun (_, (_, e)) -> e) args) in
  match ty_variadic with
  | None -> formal_args, concrete_args
  | Some {lab; att=_; typ} ->
      let arg = fresh () in
      let extra_args arg = array_of_list (list_map (ml2js_fun typ) arg) in
      let extra_args = match lab with
        | Arg | Lab _ -> extra_args (var arg)
        | Opt _ ->
            match_some_none ~none:(Exp.array []) ~some:extra_args (var arg)
      in
      formal_args @ [arg_label lab, arg], array_append concrete_args extra_args

and ml2js_unit ty_res res =
  match ty_res with
  | Unit _ -> res
  | _ -> ml2js ty_res res

and js2ml_unit ty_res res =
  match ty_res with
  | Unit _ -> app (var "ignore") [ Nolabel, res ] false
  | _ -> js2ml ty_res res

and gen_typ = function
  | Name (s, tyl) ->
      Typ.constr (mknoloc (Longident.parse s)) (List.map gen_typ tyl)
  | Js ->
      Typ.constr (mknoloc (Longident.parse "Ojs.t")) []
  | Unit _ ->
      Typ.constr (mknoloc (Lident "unit")) []
  | Arrow {ty_args; ty_vararg; unit_arg; ty_res} ->
      let tl =
        match ty_vararg with
        | None -> ty_args
        | Some {lab; att; typ} -> ty_args @ [{lab; att; typ=Name ("list", [typ])}]
      in
      let tl = if unit_arg then tl @ [{lab=Arg;att=[];typ=Unit none}] else tl in
      List.fold_right (fun {lab; att=_; typ} t2 -> Typ.arrow (arg_label lab) (gen_typ typ) t2) tl (gen_typ ty_res)
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
  | Union (_loc, cases) ->
      let f (label, params) =
        match params with
        | None -> Rtag (label, [], true, [])
        | Some typ -> Rtag (label, [], false, [gen_typ typ])
      in
      let rows = List.map f cases in
      Typ.variant rows Closed None
  | Tuple typs ->
      Typ.tuple (List.map gen_typ typs)

let rec gen_decls si =
  List.concat (List.map gen_decl si)

and gen_funs_record lbls =
  let prepare_label l =
    let js = ref l.pld_name.txt in
    List.iter
      (fun (k, v) ->
         match k.txt with
         | "js" -> register_loc k.loc; js := id_of_expr (expr_of_payload k.loc v)
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
         (gen_typ (Arrow {ty_args = [{lab=Arg;att=[];typ=Js}]; ty_vararg = None; unit_arg = false; ty_res = Name (name, [])})))
      of_js;
    Vb.mk
      ~loc:p.ptype_loc
      (Pat.constraint_
         (Pat.var (mknoloc (name ^ "_to_js")))
         (gen_typ (Arrow {ty_args = [{lab=Arg;att=[];typ=Name (name, [])}];
                          ty_vararg = None; unit_arg = false; ty_res = Js})))
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

  | Class decls ->
      let cast_funcs = List.concat (List.map gen_class_cast decls) in
      let classes = List.map (gen_classdecl cast_funcs) decls in
      [Str.class_ classes; Str.value Nonrecursive cast_funcs]

  | Implem str ->
      mapper.Ast_mapper.structure mapper str

and gen_classdecl cast_funcs = function
  | Declaration { class_name; class_fields } ->
      let x = fresh() in
      let obj =
        Cl.structure
          (Cstr.mk (Pat.any()) (List.map (gen_class_field x) class_fields))
      in
      (* generate "let _ = t_to_js in" to avoid unused decl warnings *)
      let ign = function
        | {pvb_pat = {ppat_desc = Ppat_var {txt; loc = _}; _}; _} -> Vb.mk (Pat.any ()) (var txt)
        | _ -> assert false
      in
      let obj = Cl.let_ Nonrecursive (List.map ign cast_funcs) obj in
      let obj = Cl.let_ Nonrecursive cast_funcs obj in
      Ci.mk
        (mknoloc class_name)
        (Cl.fun_ Nolabel None (Pat.constraint_ (Pat.var (mknoloc x)) ojs_typ) obj)
  | Constructor {class_name; js_class_name; class_arrow = {ty_args; ty_vararg; unit_arg; ty_res}} ->
      let formal_args, concrete_args = add_variadic_arg (gen_args ty_args) ty_vararg in
      let obj = ojs "new_obj" [ojs_variable js_class_name; concrete_args] in
      let super_class =
        match ty_res with
        | Name (super_class, []) -> super_class
        | _ -> assert false
      in
      let e = Cl.apply (Cl.constr (mknoloc (Longident.parse super_class)) []) [Nolabel, obj] in
      let e = if unit_arg then Cl.fun_ Nolabel None unit_pat e else e in
      let f e (label, x) = Cl.fun_ label None (Pat.var (mknoloc x)) e in
      Ci.mk (mknoloc class_name) (List.fold_left f e (List.rev formal_args))

and gen_class_field x = function
  | Method {method_name; method_typ; method_def; method_loc} ->
    let body =
      match method_def, method_typ with
      | Getter s, ty_res -> js2ml ty_res (ojs "get" [var x; str s])
      | Setter s, Arrow {ty_args = [{lab=Arg; att=_; typ}]; ty_vararg = None; unit_arg = false; ty_res = Unit _} ->
          mkfun (fun arg -> ojs "set" [var x; str s; ml2js typ arg])
      | MethodCall s, Arrow {ty_args; ty_vararg; unit_arg; ty_res} ->
          let formal_args, concrete_args = add_variadic_arg (gen_args ty_args) ty_vararg in
          let res = ojs "call" [var x; str s; concrete_args] in
          func formal_args unit_arg (js2ml_unit ty_res res)
      | MethodCall s, ty_res ->
          js2ml_unit ty_res (ojs "call" [var x; str s; Exp.array []])
      | _ -> error method_loc Binding_type_mismatch
    in
    Cf.method_ (mknoloc method_name) Public (Cf.concrete Fresh body)
  | Inherit super ->
    let e = Cl.apply (Cl.constr super []) [Nolabel, var x] in
    Cf.inherit_ Fresh e None

and gen_class_cast = function
  | Declaration { class_name; class_fields = _ } ->
      let class_typ = Typ.constr (mknoloc (Longident.parse class_name)) [] in
      let to_js =
        let arg = fresh() in
        Vb.mk (Pat.var (mknoloc (class_name ^ "_to_js"))) (Exp.fun_ Nolabel None (Pat.constraint_ (Pat.var (mknoloc arg)) class_typ) (Exp.constraint_ (Exp.send (var arg) "to_js") ojs_typ))
      in
      let of_js =
        let arg = fresh() in
        Vb.mk (Pat.var (mknoloc (class_name ^ "_of_js"))) (Exp.fun_ Nolabel None (Pat.constraint_ (Pat.var (mknoloc arg)) ojs_typ) (Exp.constraint_ (Exp.apply (Exp.new_ (mknoloc (Longident.Lident class_name))) [Nolabel, var arg]) class_typ))
      in
      [to_js; of_js]
  | Constructor {class_name = _; js_class_name = _; class_arrow = _} -> []

and gen_def loc decl ty =
  match decl, ty with
  | Cast, Arrow {ty_args = [{lab=Arg; att=_; typ}]; ty_vararg = None; unit_arg = false; ty_res} ->
      mkfun (fun this -> js2ml ty_res (ml2js typ this))

  | PropGet s, Arrow {ty_args = [{lab=Arg; att=_; typ}]; ty_vararg = None; unit_arg = false; ty_res} ->
      mkfun (fun this -> js2ml ty_res (ojs "get" [ml2js typ this; str s]))

  | PropSet s,
    Arrow {ty_args = [{lab=Arg; att=_; typ=(Name _ as ty_this)};
                      {lab=Arg; att=_; typ=ty_arg}];
           ty_vararg = None; unit_arg = false; ty_res = Unit _} ->
      let res this arg =
        ojs "set"
          [
            ml2js ty_this this;
            str s;
            ml2js ty_arg arg
          ]
      in
      mkfun (fun this -> mkfun (fun arg -> res this arg))

  | MethCall s,
    Arrow {ty_args = {lab=Arg; att=_; typ} :: ty_args; ty_vararg; unit_arg; ty_res} ->
      let formal_args, concrete_args = add_variadic_arg (gen_args ty_args) ty_vararg in
      let res this = ojs "call" [ ml2js typ this; str s; concrete_args ] in
      mkfun
        (fun this ->
           match ty_args, ty_vararg, unit_arg with
           | [], None, false -> js2ml_unit ty_res (res this)
           | [], _, _
           | _ :: _, _, _ -> func formal_args unit_arg (js2ml_unit ty_res (res this))
        )

  | Global s, ty_res ->
      let res = ojs_variable s in
      js2ml ty_res res

  | Expr e, Arrow {ty_args; ty_vararg = None; unit_arg = false; ty_res} -> (* TODO: handle variadic argument *)
      let args = gen_args ~name:(Printf.sprintf "arg%i") ty_args in
      func (List.map fst args) false (js2ml_unit ty_res (gen_expr loc args e))

  | Expr e, ty ->
      js2ml ty (gen_expr loc [] e)

  | New name, Arrow {ty_args; ty_vararg; unit_arg; ty_res} ->
      let formal_args, concrete_args = add_variadic_arg (gen_args ty_args) ty_vararg in
      let res = ojs "new_obj" [ojs_variable name; concrete_args] in
      func formal_args unit_arg (js2ml ty_res res)

  | Builder, Arrow {ty_args; ty_vararg = None; unit_arg; ty_res} ->
      let gen_arg {lab; att; typ} =
        let s = fresh () in
        (arg_label lab, s),
        fun x ->
          let js =
            match get_string_attribute "js" att, lab with
            | Some s, _ -> s
            | None, Arg -> error loc Unlabelled_argument_in_builder
            | None, (Lab {ml; _} | Opt {ml; _}) -> js_name ml
          in
          let code exp = ojs "set" [x; str js; ml2js typ exp] in
          (* special logic to avoid setting optional argument to 'undefined' *)
          match lab with
          | Arg | Lab _ -> code (var s)
          | Opt {def; _} ->
              begin match def with
              | None ->
                  match_some_none (var s) ~none:unit_expr ~some:code
              | Some none ->
                  code (match_some_none ~none ~some:(fun v -> v) (var s))
              end
      in

      let args = List.map gen_arg ty_args in
      let formal_args = List.map fst args in
      let concrete_args = List.map snd args in
      let f x init code = Exp.sequence (code x) init in
      let init x = List.fold_left (f x) (js2ml_unit ty_res x) (List.rev concrete_args) in
      let body = let_exp_in (ojs "empty_obj" [unit_expr]) init in
      func formal_args unit_arg body

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
  | Call (Id "global", [Str s]) -> ojs_variable s
  | Str s ->
      ml2js (Name ("string", [])) (str s)
  | Id s ->
      begin match List.find (fun ((_, name), _) -> s = name) ids with
      | (_, (_, e)) -> e
      | exception Not_found -> error loc Invalid_expression
      end
  | _ ->
      error loc Invalid_expression


(** ppx mapper *)

and str_of_sg sg =
  let decls = parse_sig sg in
  attr "comment" (str "!! This code has been generated by gen_js_api !!") ::
  disable_warnings ::
  gen_decls decls

and mapper =
  let open Ast_mapper in
  let super = default_mapper in
  let module_expr self mexp =
    let mexp = super.module_expr self mexp in
    match mexp.pmod_desc with
    | Pmod_constraint({pmod_desc=Pmod_extension ({txt="js"; loc = _}, PStr[]); _},
                      ({pmty_desc=Pmty_signature sg; _} as mty)) ->
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
    | Pexp_extension ({txt="js.to"; loc}, PTyp ty) -> register_loc loc; js2ml_fun (parse_typ ty)
    | Pexp_extension ({txt="js.of"; loc}, PTyp ty) -> register_loc loc; ml2js_fun (parse_typ ty)
    | _ ->
        e
  in
  {super with module_expr; structure_item; expr}

let check_loc_mapper =
  let mapper = Ast_mapper.default_mapper in
  let attribute _this (({txt; loc}, _) as attr) =
    if txt = "js" || has_prefix ~prefix:"js." txt then begin
      if is_registered_loc loc then ()
      else error loc Spurious_attribute
    end;
    attr
  in
  { mapper with Ast_mapper.attribute }

(** Main *)

let out = ref ""

let specs =
  [
    "-o", Arg.Set_string out, "  Specify output .ml file (- for stdout).";
  ]

let usage = "gen_js_api [-o mymodule.ml] mymodule.mli"

let standalone () =
  let files = ref [] in
  Arg.parse specs (fun s -> files := s :: !files) usage;
  let src =
    match !files with
    | [src] -> src
    | [] -> error Location.none No_input
    | _ -> error Location.none Multiple_inputs
  in
  if !out = "" then out := Filename.chop_extension src ^ ".ml";
  let oc = if !out = "-" then stdout else open_out !out in
  let sg =
    Pparse.parse_interface Format.err_formatter
      ~tool_name:"gen_js_iface"
      src
  in
  let res = str_of_sg sg in
  ignore (check_loc_mapper.Ast_mapper.signature check_loc_mapper sg);
  Format.fprintf (Format.formatter_of_out_channel oc) "%a@."
    Pprintast.structure res;
  if !out <> "-" then close_out oc

let () =
  try
    if Array.length Sys.argv < 4 || Sys.argv.(1) <> "-ppx" then standalone ()
    else
      Ast_mapper.run_main
        (fun _ ->
           { mapper with
             Ast_mapper.structure = (fun _this str -> check_loc_mapper.Ast_mapper.structure check_loc_mapper (mapper.Ast_mapper.structure mapper str)) }
        )
  with exn ->
    Format.eprintf "%a@." Location.report_exception exn;
    exit 2
