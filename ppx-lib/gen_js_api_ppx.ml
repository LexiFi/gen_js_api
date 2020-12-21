(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

open Migrate_parsetree.Ast_411

open Location
open Asttypes
open Parsetree
open Longident
open! Ast_helper

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
  | Not_supported_here of string
  | Constructor_in_union
  | Unknown_union_method
  | Non_constant_constructor_in_enum
  | Multiple_default_case
  | Invalid_variadic_type_arg
  | No_input
  | Multiple_inputs
  | Unlabelled_argument_in_builder
  | Spurious_attribute
  | Sum_kind_args
  | Union_without_discriminator
  | Contravariant_type_parameter of string
  | Missing_requried_definitions of string

exception Error of Location.t * error

let check_attribute = ref true
let mark_as_handled_manually = ref (fun (_ : Parsetree.attribute) -> ())
let used_attributes_tbl = Hashtbl.create 16

(* [merlin_hide] tells merlin to not look at a node, or at any of its
   descendants.  *)
let merlin_hide =
  { attr_name = { txt = "merlin.hide"; loc = Location.none }
  ; attr_payload = PStr []
  ; attr_loc = Location.none
  }

let register_loc attr =
  !mark_as_handled_manually attr;
  Hashtbl.replace used_attributes_tbl attr.attr_name.loc ()

let is_registered_loc loc = Hashtbl.mem used_attributes_tbl loc

let error loc err = raise (Error (loc, err))

let filter_attr_name key attr =
  if attr.attr_name.txt = key then begin
    register_loc attr;
    true
  end else false

let filter_extension key name = name.txt = key

let filter_attr key attribute  = filter_attr_name key attribute

let has_attribute key attrs = List.exists (filter_attr key) attrs

let get_attribute key attrs =
  match List.find (filter_attr key) attrs with
  | exception Not_found -> None
  | {attr_name; attr_payload; _} -> Some (attr_name, attr_payload)

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
  | {pexp_desc=Pexp_constant (Pconst_string (s, _, _)); _} -> s
  | e -> error e.pexp_loc Identifier_expected

let get_expr_attribute key attrs =
  match get_attribute key attrs with
  | None -> None
  | Some (k, v) -> Some (expr_of_payload k.loc v)

let get_string_attribute key attrs =
  match get_attribute key attrs with
  | None -> None
  | Some (k, v) -> Some (id_of_expr (expr_of_payload k.loc v))

let get_string_attribute_default key default attrs =
  match get_attribute key attrs with
  | None -> default
  | Some (k, v) -> k.loc, id_of_expr (expr_of_payload k.loc v)

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
  | Not_supported_here msg ->
      Format.fprintf ppf "%s not supported in this context" msg
  | Non_constant_constructor_in_enum ->
      Format.fprintf ppf "Constructors in enums cannot take arguments"
  | Multiple_default_case ->
      Format.fprintf ppf "At most one default constructor is supported in variants"
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
  | Sum_kind_args ->
      Format.fprintf ppf "Incompatible label name for 'kind' and constructor arguments."
  | Constructor_in_union ->
      Format.fprintf ppf "Constructors in unions must be constant or unary."
  | Unknown_union_method ->
      Format.fprintf ppf "Unknown method to discriminate unions."
  | Union_without_discriminator ->
      Format.fprintf ppf "js.union without way to discriminate values."
  | Contravariant_type_parameter label ->
      Format.fprintf ppf "Contravariant type parameter '%s is not allowed." label
  | Missing_requried_definitions label ->
      Format.fprintf ppf "'%s' must be manually declared here" label

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) -> Some (Location.error_of_printer ~loc print_error err)
      | _ -> None
    )

(*
let show_attrs attrs =
  prerr_endline "===========";
  prerr_endline "attributes:";
  List.iter (fun ({txt; loc = _}, _) -> prerr_endline txt) attrs
*)

let js_name ~global_attrs ?(capitalize = false) name =
  if has_attribute "js.verbatim_names" global_attrs then
    if capitalize then String.capitalize_ascii name
    else name
  else
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

let get_js_constr ~global_attrs name attributes =
  match get_attribute "js" attributes with
  | None -> `String (js_name ~global_attrs name)
  | Some (k, v) ->
      begin match (expr_of_payload k.loc v).pexp_desc with
      | Pexp_constant (Pconst_string (s, _, _)) -> `String s
      | Pexp_constant (Pconst_integer (n, _)) -> `Int (int_of_string n)
      | Pexp_construct (ident_loc, _) ->
          begin match ident_loc.txt with
            | Lident "true"  -> `Bool true
            | Lident "false" -> `Bool false
            | _ -> error ident_loc.loc Invalid_expression
          end
      | _ -> error k.loc Invalid_expression
      end

(** AST *)

type typ =
  | Arrow of arrow_params
  | Unit of Location.t
  | Js
  | Name of string * typ list
  | Variant of { location: Location.t;
                 global_attrs:attributes;
                 attributes:attributes;
                 constrs:constructor list }
  | Tuple of typ list
  | Typ_var of string

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

and constructor_arg =
  | Constant
  | Unary of typ
  | Nary of typ list
  | Record of (Location.t * lid * string * typ) list

and constructor =
  {
    mlconstr: string;
    arg: constructor_arg;
    attributes: attributes;
    location: Location.t;
  }

let arg_label = function
  | Arg -> Nolabel
  | Lab {ml; _} -> Labelled ml
  | Opt {ml; _} -> Optional ml

type valdef =
  | Cast
  | Ignore
  | PropGet of string
  | PropSet of string
  | MethCall of string
  | Global of string
  | New of string option
  | Builder of attributes

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
  | Type of rec_flag * Parsetree.type_declaration list * attributes
  | Val of string * typ * valdef * Location.t * attributes
  | Class of classdecl list
  | Implem of Parsetree.structure
  | Open of Parsetree.open_description

(** Parsing *)

let rec parse_arg lab ~global_attrs ty =
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
    typ = parse_typ ~global_attrs ty;
  }

and parse_typ ~global_attrs ty =
  match ty.ptyp_desc with
  | Ptyp_arrow (lab, t1, t2) when has_attribute "js.variadic" t1.ptyp_attributes ->
      begin match parse_arg lab ~global_attrs t1 with
      | {lab; att; typ=Name ("list", [typ])} ->
          let ty_vararg = Some {lab; att; typ} in
          begin match parse_typ ~global_attrs t2 with
          | Arrow ({ty_args = []; ty_vararg = None; unit_arg = _; ty_res = _} as params) when t2.ptyp_attributes = [] ->
              Arrow {params with ty_vararg}
          | Arrow _ when t2.ptyp_attributes = [] -> error ty.ptyp_loc Cannot_parse_type
          | tres -> Arrow {ty_args = []; ty_vararg; unit_arg = false; ty_res = tres}
          end
      | _ -> error t1.ptyp_loc Invalid_variadic_type_arg
      end
  | Ptyp_arrow (lab, t1, t2) ->
      let t1 = parse_arg lab ~global_attrs t1 in
      begin match parse_typ ~global_attrs t2 with
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
      | s, tl -> Name (s, List.map (parse_typ ~global_attrs) tl)
      end
  | Ptyp_variant (rows, Closed, None) ->
      let location = ty.ptyp_loc in
      let prepare_row = function
        | {prf_desc = Rtag ({txt = mlconstr; _}, true, []); prf_attributes = attributes; prf_loc = location} ->
            { mlconstr; arg = Constant; attributes; location }
        | {prf_desc = Rtag ({txt = mlconstr; _}, false, [typ]); prf_attributes = attributes; prf_loc = location} ->
            begin match parse_typ ~global_attrs typ with
            | Tuple typs -> { mlconstr; arg = Nary typs; attributes; location }
            | typ -> { mlconstr; arg = Unary typ; attributes; location }
            end
        | _ -> error location Cannot_parse_type
      in
      Variant {location; global_attrs; attributes = ty.ptyp_attributes; constrs = List.map prepare_row rows}

  | Ptyp_tuple typs ->
      let typs = List.map (parse_typ ~global_attrs) typs in
      Tuple typs

  | Ptyp_var label -> Typ_var label

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

let rec choose f = function
  | [] -> []
  | x :: xs ->
      begin match f x with
      | None -> choose f xs
      | Some y -> y :: choose f xs
      end

let derived_from_type s ty =
  match ty with
  | Arrow {ty_args; ty_vararg = None; unit_arg = false; ty_res = Js} ->
      begin match List.rev ty_args with
      | {lab=Arg; att=_; typ=Name (t, _);} :: _ -> check_suffix ~suffix:"_to_js" s = Some t
      | _ -> false
      end
  | Arrow {ty_res = Name (t, _); ty_vararg = None; unit_arg = false; ty_args } ->
      begin match List.rev ty_args with
      | {lab=Arg; att=_; typ= Js;} :: _ -> check_suffix ~suffix:"_of_js" s = Some t
      | _ -> false
      end
  | _ -> false

let auto ~global_attrs s ty =
  if derived_from_type s ty then
    Ignore
  else
    match ty with
    | Arrow {ty_args = _; ty_vararg = None; unit_arg = _; ty_res = Name _} when s = "create" -> New None
    | Arrow {ty_args = _; ty_vararg = None; unit_arg = _; ty_res = Name _} when has_prefix ~prefix:"new_" s -> New (Some (js_name ~capitalize:true ~global_attrs (drop_prefix ~prefix:"new_" s)))
    | Arrow {ty_args = [_]; ty_vararg = None; unit_arg = false; ty_res = Unit _} when has_prefix ~prefix:"set_" s -> PropSet (js_name ~global_attrs (drop_prefix ~prefix:"set_" s))
    | Arrow {ty_args = [{lab=Arg; att=_; typ=Name _}; _]; ty_vararg = None; unit_arg = false; ty_res = Unit _} when has_prefix ~prefix:"set_" s -> PropSet (js_name ~global_attrs (drop_prefix ~prefix:"set_" s))
    | Arrow {ty_args = [{lab=Arg; att=_; typ=Name _}]; ty_vararg = None; unit_arg = false; ty_res = Unit _} -> MethCall (js_name ~global_attrs s)
    | Arrow {ty_args = [{lab=Arg; att=_; typ=Name _}]; ty_vararg = None; unit_arg = false; ty_res = _} -> PropGet (js_name ~global_attrs s)
    | Arrow {ty_args = []; ty_vararg = None; unit_arg = true; ty_res = _} -> PropGet (js_name ~global_attrs s)
    | Arrow {ty_args = {lab=Arg; att=_; typ=Name _} :: _; ty_vararg = _; unit_arg = _; ty_res = _} -> MethCall (js_name ~global_attrs s)
    | _ -> Global (js_name ~global_attrs s)

let auto_in_object ~global_attrs s = function
  | Arrow {ty_args = [{lab=Arg; att=_; typ=_}]; ty_vararg = None; unit_arg = false; ty_res = Unit _} when has_prefix ~prefix:"set_" s -> PropSet (js_name ~global_attrs (drop_prefix ~prefix:"set_" s))
  | Arrow _ -> MethCall (js_name ~global_attrs s)
  | Unit _ -> MethCall (js_name ~global_attrs s)
  | _ -> PropGet (js_name ~global_attrs s)

let parse_attr ~global_attrs (s, loc, auto) attribute =
  let opt_name ?(prefix = "") ?(capitalize = false) () =
    match attribute.attr_payload with
    | PStr [] ->
        begin match check_prefix ~prefix s with
        | None -> error loc (Implicit_name prefix)
        | Some s ->
            js_name ~global_attrs ~capitalize s
        end
    | _ -> id_of_expr (expr_of_payload attribute.attr_name.loc attribute.attr_payload)
  in
  let actions =
    [ "js.cast", (fun () -> Cast);
      "js.get", (fun () -> PropGet (opt_name ()));
      "js.set", (fun () -> PropSet (opt_name ~prefix:"set_" ()));
      "js.call", (fun () -> MethCall (opt_name ()));
      "js.global", (fun () -> Global (opt_name ()));
      "js", (fun () -> auto ());
      "js.create", (fun () -> New None);
      "js.new", (fun () -> New (Some (opt_name ~prefix:"new_" ~capitalize:true ())));
      "js.builder", (fun () -> Builder global_attrs);
    ]
  in
  match List.find (fun (name, _) -> filter_attr_name name attribute) actions with
  | exception Not_found -> None
  | _, f -> Some (f ())

let parse_valdecl ~global_attrs ~in_sig vd =
  let attributes = vd.pval_attributes in
  let global_attrs = attributes @ global_attrs in
  let s = vd.pval_name.txt in
  let loc = vd.pval_loc in
  let ty = parse_typ ~global_attrs vd.pval_type in
  let auto () = auto ~global_attrs s ty in
  let defs = choose (parse_attr ~global_attrs (s, loc, auto)) attributes in
  let r =
    match defs with
    | [x] -> x
    | [] when in_sig -> auto ()
    | [] -> raise Exit
    | _ -> error loc Multiple_binding_declarations
  in
  Val (s, ty, r, loc, global_attrs)

let rec parse_sig_item ~global_attrs rest s =
  match s.psig_desc with
  | Psig_value vd when vd.pval_prim = [] ->
      parse_valdecl ~global_attrs ~in_sig:true vd :: rest ~global_attrs
  | Psig_type (rec_flag, decls) ->
      Type (rec_flag, decls, global_attrs) :: rest ~global_attrs
  | Psig_module {pmd_name = { txt = Some name; _}; pmd_type = {pmty_desc = Pmty_signature si; pmty_attributes; pmty_loc = _}; pmd_loc = _; pmd_attributes} ->
      (let global_attrs =
         push_module_attributes name pmty_attributes
           (push_module_attributes name pmd_attributes global_attrs)
       in
       Module (name, parse_sig ~global_attrs si)) :: rest ~global_attrs
  | Psig_class cs -> Class (List.map (parse_class_decl ~global_attrs) cs) :: rest ~global_attrs
  | Psig_attribute ({attr_payload = PStr str; _} as attribute) when filter_attr_name "js.implem" attribute -> Implem str :: rest ~global_attrs
  | Psig_attribute attribute ->
      let global_attrs = attribute :: global_attrs in
      rest ~global_attrs
  | Psig_open descr -> Open descr :: rest ~global_attrs
  | _ ->
      error s.psig_loc Cannot_parse_sigitem

and push_module_attributes module_name module_attributes global_attrs =
  let rec rev_append acc = function
    | ({attr_name = {txt = "js.scope"; _}; attr_payload = PStr [];  _}) as attribute :: tl ->
        rev_append ({ attribute with attr_payload = PStr [Str.eval (Exp.constant (Pconst_string (module_name, Location.none, None)))] } :: acc) tl
    |  hd :: tl -> rev_append (hd :: acc) tl
    | [] -> acc
  in
  rev_append global_attrs (List.rev module_attributes)

and parse_sig ~global_attrs = function
  | [] -> []
  | {psig_desc = Psig_attribute attribute; _} :: rest when filter_attr_name "js.stop" attribute ->
      parse_sig_verbatim ~global_attrs rest
  | {psig_desc = Psig_value vd; _} :: rest when
      has_attribute "js.custom" vd.pval_attributes ->
      let (k, v) = unoption (get_attribute "js.custom" vd.pval_attributes) in
      let str = str_of_payload k.loc v in
      Implem str :: parse_sig ~global_attrs rest
  | s :: rest ->
      parse_sig_item ~global_attrs (parse_sig rest) s

and parse_sig_verbatim ~global_attrs = function
  | [] -> []
  | {psig_desc = Psig_attribute attribute; _} :: rest when filter_attr_name "js.start" attribute -> parse_sig ~global_attrs rest
  | _ :: rest -> parse_sig_verbatim ~global_attrs rest

and parse_class_decl ~global_attrs = function
  | {pci_virt = Concrete; pci_params = []; pci_name; pci_expr = {pcty_desc = Pcty_arrow (Nolabel, {ptyp_desc = Ptyp_constr ({txt = Longident.Ldot (Lident "Ojs", "t"); loc = _}, []); ptyp_loc = _; ptyp_attributes = _; ptyp_loc_stack = _}, {pcty_desc = Pcty_signature {pcsig_self = {ptyp_desc = Ptyp_any; _}; pcsig_fields}; pcty_loc = _; pcty_attributes = _}); _}; pci_attributes; pci_loc = _} ->
      let global_attrs = pci_attributes @ global_attrs in
      let class_name = pci_name.txt in
      Declaration { class_name; class_fields = List.map (parse_class_field ~global_attrs) pcsig_fields }
  | {pci_virt = Concrete; pci_params = []; pci_name; pci_expr; pci_attributes; pci_loc} ->
      let global_attrs = pci_attributes @ global_attrs in
      let rec convert_typ = function
        | { pcty_desc = Pcty_constr (id, typs); pcty_attributes; pcty_loc } ->
            Typ.constr ~loc:pcty_loc ~attrs:pcty_attributes id typs
        | { pcty_desc = Pcty_arrow (label, typ, ct); pcty_attributes; pcty_loc } ->
            Typ.arrow ~loc:pcty_loc ~attrs:pcty_attributes label typ (convert_typ ct)
        | _ -> error pci_loc Cannot_parse_classdecl
      in
      let class_arrow =
        match parse_typ ~global_attrs (convert_typ pci_expr) with
        | Arrow ({ty_args = _; ty_vararg = _; unit_arg = _; ty_res = Name (_, [])} as params) -> params
        | (Name (_, []) as ty_res) -> {ty_args = []; ty_vararg = None; unit_arg = false; ty_res}
        | _ -> error pci_loc Cannot_parse_classdecl
      in
      let class_name = pci_name.txt in
      let js_class_name =
        match get_string_attribute "js.new" pci_attributes with
        | None -> js_name ~global_attrs ~capitalize:true class_name
        | Some s -> s
      in
      Constructor {class_name; js_class_name; class_arrow}
  | {pci_loc; _} -> error pci_loc Cannot_parse_classdecl

and parse_class_field ~global_attrs = function
  | {pctf_desc = Pctf_method ({txt = method_name; _}, Public, Concrete, typ); pctf_loc; pctf_attributes} ->
      let ty = parse_typ ~global_attrs typ in
      let auto () = auto_in_object ~global_attrs method_name ty in
      let defs = choose (parse_attr ~global_attrs (method_name, pctf_loc, auto)) pctf_attributes in
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

let longident_parse x = Longident.parse x [@@ocaml.warning "-deprecated"]

let var x = Exp.ident (mknoloc (longident_parse x))
let str s = Exp.constant (Pconst_string (s, Location.none, None))
let int n = Exp.constant (Pconst_integer (string_of_int n, None))
let bool b = Exp.construct (mknoloc (longident_parse (if b then "true" else "false"))) None
let pat_int n = Pat.constant (Pconst_integer (string_of_int n, None))
let pat_str s = Pat.constant (Pconst_string (s, Location.none, None))
let pat_bool b = Pat.construct (mknoloc (longident_parse (if b then "true" else "false"))) None

let attr s e = (Attr.mk (mknoloc s) (PStr [Str.eval e]))

let disable_warnings = Str.attribute (attr "ocaml.warning" (str "-7-32-39"))
(*  7: method overridden.
    32: unused value declarations (when *_of_js, *_to_js are not needed)
    39: unused rec flag (for *_of_js, *_to_js functions, when the
       type is not actually recursive) *)


let incl = function
  | [x] -> x
  | str -> Str.include_ (Incl.mk (Mod.structure str))

let nolabel args = List.map (function x -> Nolabel, x) args

let ojs_typ = Typ.constr (mknoloc (longident_parse "Ojs.t")) []

let ojs_var s = Exp.ident (mknoloc (Ldot (Lident "Ojs", s)))

let ojs s args = Exp.apply (ojs_var s) (nolabel args)

let ojs_null = ojs_var "null"

let list_iter f x =
  Exp.apply (Exp.ident (mknoloc (longident_parse "List.iter"))) (nolabel [f; x])

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
  Pat.construct (mknoloc (longident_parse "Some")) (Some arg)

let none_pat () =
  Pat.construct (mknoloc (longident_parse "None")) None

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

let exp_ignore res =
  apply (var "ignore") [ Nolabel, res ]
let split sep s =
  let n = String.length s in
  let rec aux start i =
    if i < n then
      if s.[i] = sep then String.sub s start (i - start) :: aux (i+1) (i+1)
      else aux start (i+1)
    else [String.sub s start (i - start)]
  in
  aux 0 0

let ojs_global = ojs_var "global"

let select_path o s =
  let rec select_path o = function
    | [] -> assert false
    | [x] -> o, x
    | x :: xs -> select_path (ojs "get" [o; str x]) xs
  in
  select_path o (split '.' s)

let get_path global_object s =
  let o, x = select_path global_object s in
  ojs "get" [o; str x]

let ojs_variable s =
  get_path ojs_global s

let set_path global_object s v =
  let o, x = select_path global_object s in
  ojs "set" [o; str x; v]

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

let ojs_apply_arr o = function
  | `Simple arr -> ojs "apply" [o; arr]
  | `Push arr ->
      (* inline Ojs.apply_arr *)
      ojs "call" [o; str "apply"; Exp.array [ ojs_null; arr ]]

let ojs_call_arr o s = function
  | `Simple arr -> ojs "call" [o; s; arr]
  | `Push arr ->
      (* inline Ojs.call_arr *)
      let_exp_in o
        (fun o ->
           ojs "call" [ojs "get" [o; s]; str "apply"; Exp.array [ o; arr ]]
        )

let ojs_new_obj_arr cl = function
  | `Simple arr -> ojs "new_obj" [cl; arr]
  | `Push arr -> ojs "new_obj_arr" [cl; arr]

let assert_false = Exp.assert_ (Exp.construct (mknoloc (longident_parse "false")) None)

let rewrite_typ_decl t =
  let t = {t with ptype_private = Public} in
  match t.ptype_manifest, t.ptype_kind with
  | None, Ptype_abstract -> {t with ptype_manifest = Some ojs_typ}
  | _ -> t

let string_typ = Name ("string", [])
let int_typ = Name ("int", [])
let bool_typ = Name ("bool", [])

let is_discriminator_type = function
  | Name (("string"|"int"|"bool"), []) -> true
  | _ -> false

let is_simple_enum params =
  let p {mlconstr = _; arg; attributes; location = _} =
    match arg with
    | Constant -> true
    | Unary arg_typ when is_discriminator_type arg_typ -> has_attribute "js.default" attributes
    | Unary _
    | Nary _
    | Record _ -> false
  in
  List.for_all p params

type union_discriminator =
  | No_discriminator
  | On_field of string

let get_variant_kind loc attrs =
  if has_attribute "js.enum" attrs then `Enum
  else if has_attribute "js.union" attrs then begin
    match get_attribute "js.union" attrs with
    | None -> assert false
    | Some (k, v) ->
        begin match v with
        | PStr [] -> `Union No_discriminator
        | _ ->
            begin match expr_of_payload k.loc v with
            | {pexp_desc = Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "on_field";_}; _}, [Nolabel, {pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _}]); _} -> `Union (On_field s)
            | _ -> error k.loc Unknown_union_method
            end
        end
  end else if has_attribute "js.sum" attrs then begin
    match get_attribute "js.sum" attrs with
    | None -> assert false
    | Some (k, v) ->
        begin match v with
        | PStr [] -> `Sum "kind"
        | _ -> `Sum (id_of_expr (expr_of_payload k.loc v))
        end
  end else error loc (Not_supported_here "Sum types without js.* attribute")

let rec js2ml ctx ty exp =
  match ty with
  | Js ->
      exp
  | Name (s, tl) ->
      let s = if builtin_type s then "Ojs." ^ s else s in
      let args = List.map (js2ml_fun ctx) tl in
      app (var (s ^ "_of_js")) (nolabel (args @ [exp])) false
  | Arrow {ty_args; ty_vararg; unit_arg; ty_res} ->
      let formal_args, concrete_args = prepare_args ctx ty_args ty_vararg in
      let res = ojs_apply_arr exp concrete_args in
      func formal_args unit_arg (js2ml_unit ctx ty_res res)
  | Unit _ ->
      app (var "Ojs.unit_of_js") (nolabel [exp]) false
  | Variant {location; global_attrs; attributes; constrs} ->
      js2ml_of_variant ctx ~variant:true location ~global_attrs attributes constrs exp
  | Tuple typs ->
      let f x =
        Exp.tuple (List.mapi (fun i typ -> js2ml ctx typ (ojs "array_get" [x; int i])) typs)
      in
      let_exp_in exp f
  | Typ_var alpha ->
      if List.mem alpha ctx then
        app (var ("__"^alpha ^ "_of_js")) (nolabel ([exp])) false
      else
        app (var ("Obj.magic")) (nolabel ([exp])) false

and js2ml_of_variant ctx ~variant loc ~global_attrs attrs constrs exp =
  let variant_kind = get_variant_kind loc attrs in
  let check_label =
    match variant_kind with
    | `Sum kind -> (fun loc label -> if label = kind then error loc Sum_kind_args)
    | _ -> (fun _ _ -> ())
  in
  let mkval =
    if variant then fun x arg -> Exp.variant x arg
    else fun x arg -> Exp.construct (mknoloc (Longident.Lident x)) arg
  in
  let f exp =
    let gen_cases (int_default, int_cases, string_default, string_cases, bool_default, bool_cases) {mlconstr; arg; attributes; location} =
      let case x =
        match get_js_constr ~global_attrs mlconstr attributes with
        | `String s -> int_default, int_cases, string_default, Exp.case (pat_str s) x :: string_cases, bool_default, bool_cases
        | `Int n -> int_default, Exp.case (pat_int n) x :: int_cases, string_default, string_cases, bool_default, bool_cases
        | `Bool b -> int_default, int_cases, string_default, string_cases, bool_default, Exp.case (pat_bool b) x :: bool_cases
      in
      let get_arg key typ = js2ml ctx typ (ojs "get" [exp; str key]) in
      match arg with
      | Constant -> case (mkval mlconstr None)
      | Unary arg_typ ->
          let otherwise() =
            match variant_kind with
            | `Enum -> error location Non_constant_constructor_in_enum
            | `Sum _ ->
                let loc, arg_field = get_string_attribute_default "js.arg" (location, "arg") attributes in
                check_label loc arg_field;
                case (mkval mlconstr (Some (get_arg arg_field arg_typ)))
            | `Union _ -> case (mkval mlconstr (Some (js2ml ctx arg_typ exp)))
          in
          let process_default defs cont =
            match get_attribute "js.default" attributes with
            | None -> otherwise()
            | Some (k, _) ->
                if List.for_all ((=) None) defs then begin
                  match variant_kind with
                  | `Enum ->
                      let x = fresh() in
                      cont (Exp.case (Pat.var (mknoloc x)) (mkval mlconstr (Some (var x))))
                  | `Sum _ | `Union _ ->
                      cont (Exp.case (Pat.any ()) (mkval mlconstr (Some (js2ml ctx arg_typ exp))))
                end else error k.loc Multiple_default_case
          in
          begin match variant_kind with
          | `Enum when arg_typ = int_typ ->
              process_default [int_default] (fun int_default -> Some int_default, int_cases, string_default, string_cases, bool_default, bool_cases)
          | `Enum when arg_typ = string_typ ->
              process_default [string_default] (fun string_default -> int_default, int_cases, Some string_default, string_cases, bool_default, bool_cases)
          | `Enum when arg_typ = bool_typ ->
              process_default [bool_default] (fun bool_default -> int_default, int_cases, string_default, string_cases, Some bool_default, bool_cases)
          | `Sum _ | `Union _ when arg_typ = Js ->
              process_default [int_default; string_default; bool_default] (fun default -> Some default, int_cases, Some default, string_cases, Some default, bool_cases)
          | _ -> otherwise()
          end
      | Nary args_typ ->
          begin match variant_kind with
          | `Enum -> error location Non_constant_constructor_in_enum
          | `Sum _ ->
              let loc, args_field = get_string_attribute_default "js.arg" (location, "arg") attributes in
              check_label loc args_field;
              let get_args key i typ = js2ml ctx typ (ojs "array_get" [ojs "get" [exp; str key]; int i]) in
              case (mkval mlconstr (Some (Exp.tuple (List.mapi (get_args args_field) args_typ))))
          | `Union _ -> error location Constructor_in_union
          end
      | Record args ->
          begin match variant_kind with
          | `Enum -> error location Non_constant_constructor_in_enum
          | `Sum _ ->
              case (mkval mlconstr (Some (Exp.record (List.map (fun (loc, mlname, jsname, typ) -> check_label loc jsname; mlname, get_arg jsname typ) args) None)))
          | `Union _ -> error location Constructor_in_union
          end
    in
    let (int_default, int_cases, string_default, string_cases, bool_default, bool_cases) = List.fold_left gen_cases (None, [], None, [], None, []) constrs in

    (* if both `true` and `false` are present, there is no need to generate the default cases for bool values *)
    let bool_default, generate_fail_pattern_for_bool =
      let check b = function
        | {pc_guard = None; pc_lhs={ppat_desc=Ppat_construct ({txt=Lident s;_}, None); _}; _} -> s = b
        | _ -> false
      in
      if List.exists (check "true") bool_cases && List.exists (check "false") bool_cases then None, false
      else bool_default, true
    in

    let gen_match ~fail_pattern e default other_cases =
      match default, other_cases with
      | None, [] -> None
      | Some default, _ ->
          let cases = List.rev (default :: other_cases) in
          Some (Exp.match_ e cases)
      | None, _ :: _ ->
          let cases =
            if fail_pattern then (Exp.case (Pat.any()) assert_false) :: other_cases else other_cases
          in
          Some (Exp.match_ e (List.rev cases))
    in
    let discriminator =
      match variant_kind with
      | `Enum -> exp
      | `Sum kind -> ojs "get" [exp; str kind]
      | `Union No_discriminator -> error loc Union_without_discriminator
      | `Union (On_field kind) -> ojs "get" [exp; str kind]
    in
    let int_match = gen_match ~fail_pattern:true (js2ml ctx int_typ discriminator) int_default int_cases in
    let string_match = gen_match ~fail_pattern:true (js2ml ctx string_typ discriminator) string_default string_cases in
    let bool_match = gen_match ~fail_pattern:generate_fail_pattern_for_bool (js2ml ctx bool_typ discriminator) bool_default bool_cases in
    match int_match, string_match, bool_match with
    | None, None, None -> assert false
    | Some int_match, None, None -> int_match
    | None, Some string_match, None -> string_match
    | None, None, Some bool_match -> bool_match
    | _ ->
        let case_int = Option.map (Exp.case (pat_str "number")) int_match in
        let case_string = Option.map (Exp.case (pat_str "string")) string_match in
        let case_bool = Option.map (Exp.case (pat_str "boolean")) bool_match in
        let case_default =
          match variant_kind, int_default, string_default, bool_default with
          | `Enum, _, _, _
          | _, None, None, None -> Exp.case (Pat.any ()) assert_false
          | (`Sum _ | `Union _), _, _, _ ->
              let defaults = List.filter_map Fun.id [int_default; string_default; bool_default] in
              match defaults with
              | def :: rest when List.for_all ((=) def) rest -> def
              | _ -> assert false
        in
        let cases =
          List.fold_left
            (fun state -> function Some x -> x :: state | None -> state)
            [case_default]
            [case_bool; case_string; case_int]
        in
        Exp.match_ (ojs "type_of" [discriminator]) cases
  in
  let_exp_in exp f

and ml2js ctx ty exp =
  match ty with
  | Js ->
      exp
  | Name (s, tl) ->
      let s = if builtin_type s then "Ojs." ^ s else s in
      let args = List.map (ml2js_fun ctx) tl in
      app (var (s ^ "_to_js")) (nolabel (args @ [exp])) false
  | Arrow {ty_args; ty_vararg = None; unit_arg; ty_res} ->
      let args =
        let f _i {lab; att=_; typ} =
          let s = fresh() in
          let typ =
            match lab with
            | Arg | Lab _ -> typ
            | Opt _ -> Name ("option", [typ])
          in
          s, (arg_label lab, js2ml ctx typ (var s))
        in
        List.mapi f ty_args
      in
      let formal_args, concrete_args = List.map fst args, List.map snd args in
      let res = ml2js_unit ctx ty_res (app exp concrete_args unit_arg) in
      let body = if formal_args = [] then Exp.fun_ Nolabel None (Pat.any ()) res else res in
      let f = List.fold_right (fun s -> fun_ (Nolabel, s)) formal_args body in
      ojs "fun_to_js" [int (max 1 (List.length formal_args)); f]
  | Arrow {ty_args; ty_vararg = Some {lab=label_variadic; att=_; typ=ty_variadic};
           unit_arg; ty_res} ->
      let arguments = fresh() in
      let n_args = List.length ty_args in
      let concrete_args =
        List.mapi
          (fun i {lab; att=_; typ} -> arg_label lab, js2ml ctx typ (ojs "array_get" [var arguments; int i])) ty_args
      in
      let extra_arg = ojs "list_of_js_from" [ js2ml_fun ctx ty_variadic; var arguments; int n_args ] in
      let extra_arg =
        match label_variadic with
        | Arg | Lab _ -> extra_arg
        | Opt _ -> Exp.construct (mknoloc (longident_parse "Some")) (Some extra_arg)
      in
      let concrete_args = concrete_args @ [arg_label label_variadic, extra_arg] in
      let res = app exp concrete_args unit_arg in
      let f = func [Nolabel, arguments] false (ml2js_unit ctx ty_res res) in
      ojs "fun_to_js_args" [f]
  | Unit _ -> app (var "Ojs.unit_to_js") (nolabel [exp]) false
  | Variant {location; global_attrs; attributes; constrs} ->
      ml2js_of_variant ctx ~variant:true location ~global_attrs attributes constrs exp
  | Tuple typs ->
      let typed_vars = List.mapi (fun i typ -> i, typ, fresh ()) typs in
      let pat = Pat.tuple (List.map (function (_, _, x) -> Pat.var (mknoloc x)) typed_vars) in
      Exp.let_ Nonrecursive [Vb.mk pat exp] begin
        let n = List.length typs in
        let a = fresh () in
        let new_array = ojs "array_make" [int n] in
        Exp.let_ Nonrecursive [Vb.mk (Pat.var (mknoloc a)) new_array] begin
          let f e (i, typ, x) =
            Exp.sequence (ojs "array_set" [var a; int i; ml2js ctx typ (var x)]) e
          in
          List.fold_left f (var a) (List.rev typed_vars)
        end
      end
  | Typ_var alpha ->
      if List.mem alpha ctx then
        app (var ("__"^alpha ^ "_to_js")) (nolabel ([exp])) false
      else
        app (var ("Obj.magic")) (nolabel ([exp])) false

and ml2js_discriminator ctx ~global_attrs mlconstr attributes =
  match get_js_constr ~global_attrs mlconstr attributes with
  | `Int n -> ml2js ctx int_typ (int n)
  | `String s -> ml2js ctx string_typ (str s)
  | `Bool b -> ml2js ctx bool_typ (bool b)

and ml2js_of_variant ctx ~variant loc ~global_attrs attrs constrs exp =
  let variant_kind = get_variant_kind loc attrs in
  let check_label =
    match variant_kind with
    | `Sum kind -> (fun loc label -> if label = kind then error loc Sum_kind_args)
    | _ -> (fun _ _ -> ())
  in
  let mkpat =
    if variant then fun x arg -> Pat.variant x arg
    else fun x arg -> Pat.construct (mknoloc (Longident.Lident x)) arg
  in
  let pair key typ value = Exp.tuple [str key; ml2js ctx typ value] in
  let case {mlconstr; arg; attributes; location} =
    let mkobj args =
      let discriminator = ml2js_discriminator ctx ~global_attrs mlconstr attributes in
      match variant_kind, args with
      | `Enum, [] -> discriminator
      | `Enum, _ :: _ -> error location Non_constant_constructor_in_enum
      | `Sum kind, _ -> ojs "obj" [Exp.array ((Exp.tuple [str kind; discriminator]) :: args)]
      | `Union _, [] -> ojs_null
      | `Union _, _ :: _ -> error location Constructor_in_union
    in
    match arg with
    | Constant -> Exp.case (mkpat mlconstr None) (mkobj [])
    | Unary arg_typ ->
        let x = fresh() in
        let value =
          match variant_kind with
          | `Enum when
              is_discriminator_type arg_typ &&
              has_attribute "js.default" attributes -> ml2js ctx arg_typ (var x)
          | `Enum | `Sum _ ->
              let loc, arg_field = get_string_attribute_default "js.arg" (location, "arg") attributes in
              check_label loc arg_field;
              mkobj [pair arg_field arg_typ (var x)]
          | `Union _ -> ml2js ctx arg_typ (var x)
        in
        Exp.case (mkpat mlconstr (Some (Pat.var (mknoloc x)))) value
    | Nary args_typ ->
        let loc, args_field = get_string_attribute_default "js.arg" (location, "arg") attributes in
        check_label loc args_field;
        let xis = List.mapi (fun i typ -> i, typ, fresh()) args_typ in
        let n_args = List.length xis in
        Exp.case
          (mkpat mlconstr (Some (Pat.tuple (List.map (fun (_, _, xi) -> Pat.var (mknoloc xi)) xis))))
          (let args = fresh() in
           Exp.let_ Nonrecursive
             [Vb.mk (Pat.var (mknoloc args)) (ojs "array_make" [int n_args])]
             (List.fold_left
                (fun e (i, typi, xi) ->
                   Exp.sequence
                     (ojs "array_set" [var args; int i; ml2js ctx typi (var xi)]) e)
                (mkobj [pair args_field Js (var args)])
                xis))
    | Record args ->
        let x = fresh() in
        Exp.case
          (mkpat mlconstr (Some (Pat.var (mknoloc x))))
          (mkobj (List.map (fun (loc, mlname, jsname, typ) -> check_label loc jsname; pair jsname typ (Exp.field (var x) mlname)) args))
  in
  Exp.match_ exp (List.map case constrs)

and js2ml_fun ctx ty = mkfun (js2ml ctx ty)
and ml2js_fun ctx ty = mkfun (ml2js ctx ty)

and prepare_args ctx ty_args ty_vararg =
  if ty_vararg = None &&
     List.for_all
       (function
         | {typ = Variant {location = _; global_attrs = _; attributes; constrs}; _} when has_attribute "js.enum" attributes -> is_simple_enum constrs
         | {lab = Arg | Lab _ | Opt {def = Some _; _}; _} -> true
         | {lab = Opt {def = None; _}; _} -> false
       )
       ty_args
  then
    let x,y = prepare_args_simple ctx ty_args in
    x, `Simple y
  else
    let x, y = prepare_args_push ctx ty_args ty_vararg in
    x, `Push y

and prepare_args_simple ctx ty_args =
  let f {lab; att=_; typ} =
    let s = fresh () in
    let e =
      match lab with
      | Arg | Lab _ -> ml2js ctx typ (var s)
      | Opt {def; _} ->
          begin match def with
          | None -> assert false
          | Some none ->
              ml2js ctx typ (match_some_none ~none ~some:(fun v -> v) (var s))
          end
    in
    (arg_label lab, s), e
  in
  let formal_args, concrete = List.split (List.map f ty_args) in
  let concrete_args = Exp.array concrete in
  formal_args, concrete_args

and prepare_args_push ctx ty_args ty_vararg =
  let push arr typ x =
    match typ with
    | Variant {location = _; global_attrs; attributes; constrs} when
        (has_attribute "js.enum" attributes && not (is_simple_enum constrs)) ->
        let f {mlconstr; arg; attributes; location = _} =
          let gen_tuple typs =
            let xis = List.map (fun typ -> typ, fresh ()) typs in
            let cargs =
              match xis with
              | [] -> None
              | [_, x] -> Some (Pat.var (mknoloc x))
              | _ :: _ :: _ -> Some (Pat.tuple (List.map (fun (_, xi) -> Pat.var (mknoloc xi)) xis))
            in
            let args =
              ml2js_discriminator ctx ~global_attrs mlconstr attributes ::
              List.map (fun (typi, xi) -> ml2js ctx typi (var xi)) xis
            in
            let e = exp_ignore (ojs "call" [arr; str "push"; Exp.array args]) in
            Exp.case (Pat.variant mlconstr cargs) e
          in
          match arg with
          | Constant -> gen_tuple []
          | Unary typ -> gen_tuple [typ]
          | Nary typs -> gen_tuple typs
          | Record _ -> assert false
        in
        let cases = List.map f constrs in
        Exp.match_ (Exp.constraint_ x (gen_typ typ)) cases
    | typ -> exp_ignore (ojs "call" [arr; str "push"; Exp.array [ml2js ctx typ x]])
  in
  let f {lab; att=_; typ} =
    let s = fresh () in
    (arg_label lab, s),
    fun arr ->
      let s = var s in
      match lab with
      | Arg | Lab _ -> push arr typ s
      | Opt {def; _} ->
          begin match def with
          | None ->
              match_some_none ~none:unit_expr ~some:(fun s -> push arr typ s) s
          | Some none ->
              push arr typ (match_some_none ~none ~some:(fun v -> v) s)
          end
  in
  let formal_args, concrete_args = List.split (List.map f ty_args) in
  let formal_args, concrete_args =
    match ty_vararg with
    | None -> formal_args, concrete_args
    | Some {lab; att=_; typ} ->
        let arg = fresh () in
        formal_args @ [arg_label lab, arg],
        concrete_args @ [fun arr ->
            let extra_args = list_iter (mkfun (fun x -> push arr typ x)) in
            match lab with
            | Arg | Lab _ -> extra_args (var arg)
            | Opt _ -> match_some_none ~none:unit_expr ~some:extra_args (var arg)
          ]
  in
  let body arr = List.fold_right (fun code -> Exp.sequence (code arr)) concrete_args arr in
  formal_args, let_exp_in (ojs "new_obj" [ojs_variable "Array"; Exp.array []]) body

and ml2js_unit ctx ty_res res =
  match ty_res with
  | Unit _ -> res
  | _ -> ml2js ctx ty_res res

and js2ml_unit ctx ty_res res =
  match ty_res with
  | Unit _ -> exp_ignore res
  | _ -> js2ml ctx ty_res res

and gen_typ = function
  | Name (s, tyl) ->
      Typ.constr (mknoloc (longident_parse s)) (List.map gen_typ tyl)
  | Js -> ojs_typ
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
  | Variant {location = _; global_attrs = _; attributes = _; constrs} ->
      let f {mlconstr; arg; attributes = _; location = _} =
        let mlconstr = Location.mknoloc mlconstr in
        match arg with
        | Constant -> Rf.mk (Rtag (mlconstr, true, []))
        | Unary typ -> Rf.mk (Rtag (mlconstr, false, [gen_typ typ]))
        | Nary typs -> Rf.mk (Rtag (mlconstr, false, [gen_typ (Tuple typs)]))
        | Record _ -> assert false
      in
      let rows = List.map f constrs in
      Typ.variant rows Closed None
  | Tuple typs ->
      Typ.tuple (List.map gen_typ typs)
  | Typ_var label -> Typ.var label

let process_fields ~global_attrs l =
  let loc = l.pld_name.loc in
  let mlname = l.pld_name.txt in
  let attrs = l.pld_attributes in
  let typ = l.pld_type in
  let jsname =
    match get_string_attribute "js" attrs with
    | None -> js_name ~global_attrs mlname
    | Some s -> s
  in
  loc,
  mknoloc (Lident mlname), (* OCaml label *)
  jsname, (* JS name *)
  parse_typ ~global_attrs typ

let neg_variance =function
  | -1 -> 1
  | 0 | 1 -> -1
  | _ -> invalid_arg "neg_variance"

let thorough_exists f l =
  List.fold_left (fun acc x -> acc || f x) false l

let rec typvar_occurs loc variance label = function
  | Typ_var l ->
      if label = l then
        if variance < 0 then
          error loc (Contravariant_type_parameter l)
        else
          true
      else
        false
  | Arrow {ty_args; ty_vararg; ty_res; _} ->
      let (|||) a b = a || b in
      thorough_exists (fun {typ; _} -> typvar_occurs loc (neg_variance variance) label typ) ty_args
      ||| (match ty_vararg with None -> false | Some {typ; _} -> typvar_occurs loc variance label typ)
      ||| typvar_occurs loc variance label ty_res
  | Js | Unit _ -> false
  | Name (_, l) | Tuple l -> thorough_exists (typvar_occurs loc variance label) l
  | Variant {constrs; _} -> typvar_occurs_constructors loc variance label constrs
and typvar_occurs_constructors loc variance label =
  thorough_exists (fun {arg; _} ->
      match arg with
      | Constant -> false
      | Unary typ -> typvar_occurs loc variance label typ
      | Nary l -> thorough_exists (typvar_occurs loc variance label) l
      | Record l -> thorough_exists (fun (_, _, _, typ) -> typvar_occurs loc variance label typ) l
    )

let global_object ~global_attrs =
  let rec traverse = function
    | [] -> ojs_global
    | hd :: tl ->
        begin match get_expr_attribute "js.scope" [hd] with
        | None -> traverse tl
        | Some {pexp_desc=Pexp_constant (Pconst_string (prop, _, _)); _} -> ojs "get" [(traverse tl); str prop]
        | Some global_object -> global_object
        end
  in
  traverse global_attrs

let rec gen_decls si =
  List.concat (List.map gen_decl si)

and gen_funs ~global_attrs p =
  let name = p.ptype_name.txt in
  let global_attrs = p.ptype_attributes @ global_attrs in
  let ctx =
    List.map (function
        | {ptyp_desc = Ptyp_any; ptyp_loc = _; ptyp_attributes = _; ptyp_loc_stack = _}, Invariant ->
            fresh ()
        | {ptyp_desc = Ptyp_var label; ptyp_loc = _; ptyp_attributes = _; ptyp_loc_stack = _}, Invariant ->
            label
        | _ -> error p.ptype_loc Cannot_parse_type
      ) p.ptype_params
  in
  let loc = p.ptype_loc in
  let typvar_used, of_js, to_js, custom_funs =
    match p.ptype_kind with
    | _ when has_attribute "js.custom" global_attrs ->
        let result =
          match get_attribute "js.custom" global_attrs with
          | Some (k, PStr structure) ->
              let expected_names = [ name ^ "_of_js"; name ^ "_to_js" ] in
              let unexpected_error_txt =
                Printf.sprintf "any definitions other than '%s_of_js' or '%s_to_js'" name name
              in

              let get_value_bindings (state: value_binding list) (si: structure_item) =
                match si.pstr_desc with
                | Pstr_value (_, vbs) -> vbs @ state
                | _ ->
                  raise (Error (si.pstr_loc, Not_supported_here unexpected_error_txt))
              in

              let assert_functions (vbs: value_binding list) =
                let check req_labels vb =
                  match vb.pvb_pat.ppat_desc with
                  | Ppat_var nameloc ->
                    if not (List.mem nameloc.txt req_labels) then
                      raise (Error (nameloc.loc, Not_supported_here unexpected_error_txt))
                    else
                      List.filter (fun label -> label <> nameloc.txt) req_labels
                  | _ ->
                    raise (Error (vb.pvb_loc, Not_supported_here unexpected_error_txt))
                in
                let missing_functions = List.fold_left check expected_names vbs in
                List.iter (fun label -> raise (Error (k.loc, Missing_requried_definitions label))) missing_functions
              in

              let vbs = List.fold_left get_value_bindings [] structure in
              assert_functions vbs;
              (fun _ -> true),
              lazy (raise (Error (loc, Union_without_discriminator))),
              lazy (raise (Error (loc, Union_without_discriminator))),
              vbs
          | _ -> error loc Structure_expected
        in result
    | Ptype_abstract ->
        let ty =
          match p.ptype_manifest with
          | None -> Js
          | Some ty -> parse_typ ~global_attrs ty
        in
        (fun label -> typvar_occurs loc 0 label ty),
        lazy (js2ml_fun ctx ty),
        lazy (ml2js_fun ctx ty),
        []
    | Ptype_variant cstrs ->
        let prepare_constructor c =
          let mlconstr = c.pcd_name.txt in
          let arg =
            match c.pcd_args with
            | Pcstr_tuple args ->
                begin match args with
                | [] -> Constant
                | [x] -> Unary (parse_typ ~global_attrs x)
                | _ :: _ :: _ -> Nary (List.map (parse_typ ~global_attrs) args)
                end
            | Pcstr_record args ->
                let global_attrs = c.pcd_attributes @ global_attrs in
                Record (List.map (process_fields ~global_attrs) args)
          in
          { mlconstr; arg; attributes = c.pcd_attributes; location = c.pcd_loc }
        in
        let attrs = p.ptype_attributes in
        let params = List.map prepare_constructor cstrs in
        (fun label -> typvar_occurs_constructors loc 0 label params),
        lazy (mkfun (js2ml_of_variant ctx ~variant:false loc ~global_attrs attrs params)),
        lazy (mkfun (ml2js_of_variant ctx ~variant:false loc ~global_attrs attrs params)),
        []
    | Ptype_record lbls ->
        let global_attrs = p.ptype_attributes @ global_attrs in
        let lbls = List.map (process_fields ~global_attrs) lbls in
        let of_js x (_loc, ml, js, ty) = ml, js2ml ctx ty (ojs "get" [x; str js]) in
        let to_js x (_loc, ml, js, ty) = Exp.tuple [str js; ml2js ctx ty (Exp.field x ml)] in
        (fun label -> List.exists (fun (_, _, _, ty) -> typvar_occurs loc 0 label ty) lbls),
        lazy (mkfun (fun x -> Exp.record (List.map (of_js x) lbls) None)),
        lazy (mkfun (fun x -> ojs "obj" [Exp.array (List.map (to_js x) lbls)])),
        []
    | _ ->
        error p.ptype_loc Cannot_parse_type
  in
  let force_opt x = try (Some (Lazy.force x)) with Error (_, Union_without_discriminator) -> None in
  let of_js, to_js = force_opt of_js, force_opt to_js in
  let alpha_of_js label =
    Arrow {ty_args = [{lab=Arg; att=[]; typ = Js}]; ty_vararg = None; unit_arg = false; ty_res = Typ_var label}
  in
  let alpha_to_js label =
    Arrow {ty_args = [{lab=Arg; att=[]; typ = Typ_var label}]; ty_vararg = None; unit_arg = false; ty_res = Js}
  in
  let push_typ f l =
    List.map f ctx @ l
  in
  let push_fun suffix acc =
    match acc with
    | None -> None
    | Some acc ->
        Some
          (List.fold_left
             (fun acc label ->
                let name = if typvar_used label then "__"^label^suffix else "_" in
                fun_ (Nolabel, name) acc
             ) acc (List.rev ctx)
          )
  in
  let f (name, input_typs, ret_typ, code) =
    match code with
    | None -> None
    | Some code ->
        Some
          (Vb.mk ~loc:p.ptype_loc
             (Pat.constraint_
                (Pat.var (mknoloc name))
                (gen_typ (Arrow {ty_args = (List.map (fun typ -> {lab=Arg; att=[]; typ}) input_typs); ty_vararg = None; unit_arg = false; ty_res = ret_typ})))
             code)
  in
  let funs =
    choose f [ name ^ "_of_js", push_typ alpha_of_js [Js], Name (name, List.map (fun x -> Typ_var x) ctx), push_fun "_of_js" of_js;
               name ^ "_to_js", push_typ alpha_to_js [Name (name, List.map (fun x -> Typ_var x) ctx)], Js, push_fun "_to_js" to_js ]
  in
  funs @ custom_funs

and gen_decl = function
  | Type (rec_flag, decls, global_attrs) ->
      let decls = List.map rewrite_typ_decl decls in
      let funs = List.concat (List.map (gen_funs ~global_attrs) decls) in
      [ Str.type_ rec_flag decls; Str.value rec_flag funs ]

  | Module (s, decls) ->
      [ Str.module_ (Mb.mk (mknoloc (Some s)) (Mod.structure (gen_decls decls))) ]

  | Val (_, _, Ignore, _, _) -> []

  | Val (s, ty, decl, loc, global_attrs) ->
      let global_object = global_object ~global_attrs in
      let d = gen_def ~global_object loc decl ty in
      [ def s (gen_typ ty) d ]

  | Class decls ->
      let cast_funcs = List.concat (List.map gen_class_cast decls) in
      let classes = List.map (gen_classdecl cast_funcs) decls in
      [Str.class_ classes; Str.value Nonrecursive cast_funcs]

  | Implem str ->
      mapper.Ast_mapper.structure mapper str

  | Open descr ->
      let descr = {descr with popen_expr = Mod.ident descr.popen_expr} in
      [ Str.open_ descr ]

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
      let formal_args, concrete_args = prepare_args [] ty_args ty_vararg in
      let obj = ojs_new_obj_arr (ojs_variable js_class_name) concrete_args in
      let super_class =
        match ty_res with
        | Name (super_class, []) -> super_class
        | _ -> assert false
      in
      let e = Cl.apply (Cl.constr (mknoloc (longident_parse super_class)) []) [Nolabel, obj] in
      let e = if unit_arg then Cl.fun_ Nolabel None unit_pat e else e in
      let f e (label, x) = Cl.fun_ label None (Pat.var (mknoloc x)) e in
      Ci.mk (mknoloc class_name) (List.fold_left f e (List.rev formal_args))

and gen_class_field x = function
  | Method {method_name; method_typ; method_def; method_loc} ->
      let body =
        match method_def, method_typ with
        | Getter s, ty_res -> js2ml [] ty_res (ojs "get" [var x; str s])
        | Setter s, Arrow {ty_args = [{lab=Arg; att=_; typ}]; ty_vararg = None; unit_arg = false; ty_res = Unit _} ->
            mkfun (fun arg -> ojs "set" [var x; str s; ml2js [] typ arg])
        | MethodCall s, Arrow {ty_args; ty_vararg; unit_arg; ty_res} ->
            let formal_args, concrete_args = prepare_args [] ty_args ty_vararg in
            let res = ojs_call_arr (var x) (str s) concrete_args in
            func formal_args unit_arg (js2ml_unit [] ty_res res)
        | MethodCall s, ty_res ->
            js2ml_unit [] ty_res (ojs "call" [var x; str s; Exp.array []])
        | _ -> error method_loc Binding_type_mismatch
      in
      Cf.method_ (mknoloc method_name) Public (Cf.concrete Fresh (Exp.constraint_ body (gen_typ method_typ)))
  | Inherit super ->
      let e = Cl.apply (Cl.constr super []) [Nolabel, var x] in
      Cf.inherit_ Fresh e None

and gen_class_cast = function
  | Declaration { class_name; class_fields = _ } ->
      let class_typ = Typ.constr (mknoloc (longident_parse class_name)) [] in
      let to_js =
        let arg = fresh() in
        Vb.mk (Pat.var (mknoloc (class_name ^ "_to_js")))
          (Exp.fun_ Nolabel None
             (Pat.constraint_ (Pat.var (mknoloc arg)) class_typ)
             (Exp.constraint_ (Exp.send (var arg) (mknoloc "to_js")) ojs_typ))
      in
      let of_js =
        let arg = fresh() in
        Vb.mk (Pat.var (mknoloc (class_name ^ "_of_js")))
          (Exp.fun_ Nolabel None
             (Pat.constraint_ (Pat.var (mknoloc arg)) ojs_typ)
             (Exp.constraint_ (Exp.apply (Exp.new_ (mknoloc (Longident.Lident class_name))) [Nolabel, var arg]) class_typ))
      in
      [to_js; of_js]
  | Constructor {class_name = _; js_class_name = _; class_arrow = _} -> []

and gen_def ~global_object loc decl ty =
  match decl, ty with
  | Cast, Arrow {ty_args = [{lab=Arg; att=_; typ}]; ty_vararg = None; unit_arg = false; ty_res} ->
      mkfun (fun this -> js2ml [] ty_res (ml2js [] typ this))

  | PropGet s, Arrow {ty_args = [{lab=Arg; att=_; typ}]; ty_vararg = None; unit_arg = false; ty_res} ->
      mkfun (fun this -> js2ml [] ty_res (ojs "get" [ml2js [] typ this; str s]))

  | PropGet s, Arrow {ty_args = []; ty_vararg = None; unit_arg = true; ty_res} ->
      fun_unit (gen_def ~global_object loc (Global s) ty_res)

  | Global s, ty_res ->
      begin match ty_res with
      | Arrow {ty_args; ty_vararg; unit_arg; ty_res} ->
          let this, s = select_path global_object s in
          let formal_args, concrete_args = prepare_args [] ty_args ty_vararg in
          let res this = ojs_call_arr (ml2js [] Js this) (str s) concrete_args in
          func formal_args unit_arg (js2ml_unit [] ty_res (res this))
      | _ -> js2ml [] ty_res (get_path global_object s)
      end

  | PropSet s,
    Arrow {ty_args = [{lab=Arg; att=_; typ=(Name _ as ty_this)};
                      {lab=Arg; att=_; typ=ty_arg}];
           ty_vararg = None; unit_arg = false; ty_res = Unit _} ->
      let res this arg =
        ojs "set"
          [
            ml2js [] ty_this this;
            str s;
            ml2js [] ty_arg arg
          ]
      in
      mkfun (fun this -> mkfun (fun arg -> res this arg))

  | PropSet s, Arrow {ty_args = [{lab = Arg; att = _; typ = ty_arg}]; ty_vararg = None; unit_arg = false; ty_res = Unit _} ->
      mkfun (fun arg -> set_path global_object s (ml2js [] ty_arg arg))

  | MethCall s,
    Arrow {ty_args = {lab=Arg; att=_; typ} :: ty_args; ty_vararg; unit_arg; ty_res} ->
      let formal_args, concrete_args = prepare_args [] ty_args ty_vararg in
      let res this = ojs_call_arr (ml2js [] typ this) (str s) concrete_args in
      mkfun
        (fun this ->
           match ty_args, ty_vararg, unit_arg with
           | [], None, false -> js2ml_unit [] ty_res (res this)
           | [], _, _
           | _ :: _, _, _ -> func formal_args unit_arg (js2ml_unit [] ty_res (res this))
        )

  | New name, Arrow {ty_args; ty_vararg; unit_arg; ty_res} ->
      let formal_args, concrete_args = prepare_args [] ty_args ty_vararg in
      let res =
        let constructor =
          match name with
          | None -> global_object
          | Some name -> get_path global_object name
        in
        ojs_new_obj_arr constructor concrete_args
      in
      func formal_args unit_arg (js2ml [] ty_res res)

  | Builder global_attrs, Arrow {ty_args; ty_vararg = None; unit_arg; ty_res} ->
      let gen_arg {lab; att; typ} =
        let s = fresh () in
        (arg_label lab, s),
        fun x ->
          let js =
            match get_string_attribute "js" att, lab with
            | Some s, _ -> s
            | None, Arg -> error loc Unlabelled_argument_in_builder
            | None, (Lab {ml; _} | Opt {ml; _}) -> js_name ~global_attrs ml
          in
          let code exp = ojs "set" [x; str js; ml2js [] typ exp] in
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
      let init x = List.fold_left (f x) (js2ml_unit [] ty_res x) (List.rev concrete_args) in
      let body = let_exp_in (ojs "empty_obj" [unit_expr]) init in
      func formal_args unit_arg body

  | _ ->
      error loc Binding_type_mismatch


(** ppx mapper *)

and str_of_sg ~global_attrs sg =
  let decls = parse_sig ~global_attrs sg in
  let attr =
    attr "js.dummy" (str "!! This code has been generated by gen_js_api !!")
  in
  register_loc attr;
  Str.attribute attr ::
  disable_warnings ::
  gen_decls decls

and module_expr_rewriter ~loc ~attrs sg =
  Mod.constraint_
    (Mod.structure ~attrs:[ merlin_hide ] (str_of_sg ~global_attrs:(attrs) sg))
    (Mty.signature ~loc ~attrs sg)

and js_to_rewriter ~loc ty =
  let e' = with_default_loc {loc with loc_ghost = true }
      (fun () -> js2ml_fun [] (parse_typ ~global_attrs:[] ty))
  in
  { e' with pexp_loc = loc }

and js_of_rewriter ~loc ty =
  let e' = with_default_loc {loc with loc_ghost = true}
      (fun () -> ml2js_fun [] (parse_typ ~global_attrs:[] ty))
  in
  { e' with pexp_loc = loc  }

and type_decl_rewriter ~loc rec_flag l =
  let itm = with_default_loc {loc with loc_ghost = true}
      (fun () ->
         let funs = List.concat (List.map (gen_funs ~global_attrs:[]) l) in
         [
           disable_warnings;
           Str.value ~loc:loc rec_flag funs
         ]
      )
  in
  itm

and mapper =
  let open Ast_mapper in
  let super = default_mapper in

  let module_expr self mexp =
    let mexp = super.module_expr self mexp in
    match mexp.pmod_desc with
    | Pmod_extension ({txt = "js"; _}, PSig sg) ->
        module_expr_rewriter ~loc:mexp.pmod_loc ~attrs:mexp.pmod_attributes sg
    | _ -> mexp
  in
  let structure_item self str =
    let str = super.structure_item self str in
    let global_attrs = [] in
    match str.pstr_desc with
    | Pstr_primitive vd when vd.pval_prim = [] ->
        begin match parse_valdecl ~global_attrs ~in_sig:false vd with
        | exception Exit -> str
        | d -> incl (gen_decls [d])
        end
    | Pstr_type (rec_flag, decls) ->
        let js_decls = List.filter (fun d -> has_attribute "js" d.ptype_attributes) decls in
        begin match js_decls with
        | [] -> str
        | l ->
            incl (
              {str with pstr_desc = Pstr_type (rec_flag, List.map (fun d -> if has_attribute "js" d.ptype_attributes then rewrite_typ_decl d else d) decls)}
              ::
              type_decl_rewriter ~loc:str.pstr_loc rec_flag l
            )
        end
    | _ ->
        str
  in
  let expr self e =
    let e = super.expr self e in
    match e.pexp_desc with
    | Pexp_extension (attr, PTyp ty) when filter_extension "js.to" attr ->
        js_to_rewriter ~loc:e.pexp_loc ty
    | Pexp_extension (attr, PTyp ty) when filter_extension "js.of" attr ->
        js_of_rewriter ~loc:e.pexp_loc ty
    | _ ->
        e
  in
  let attribute self a =
    ignore (filter_attr "js.dummy" a : bool);
    super.attribute self a
  in
  {super with module_expr; structure_item; expr; attribute}

let is_js_attribute txt = txt = "js" || has_prefix ~prefix:"js." txt

let check_loc_mapper =
  let mapper = Ast_mapper.default_mapper in
  let attribute _this ({attr_name = {txt; loc}; _} as attr) =
    if is_js_attribute txt then begin
      if is_registered_loc loc || not !check_attribute then ()
      else error loc Spurious_attribute
    end;
    attr
  in
  { mapper with Ast_mapper.attribute }

let clear_attr_mapper =
  let mapper = Ast_mapper.default_mapper in
  let attributes _this attrs =
    let f {attr_name = {txt = _; loc}; _} = not (is_registered_loc loc) in
    List.filter f attrs
  in
  { mapper with Ast_mapper.attributes }

(** Main *)

let out = ref ""

let specs =
  [
    "-o", Arg.Set_string out, "  Specify output .ml file (- for stdout).";
  ]

let usage = "gen_js_api [-o mymodule.ml] mymodule.mli"

let from_current =
  let open Migrate_parsetree in
  Versions.migrate Versions.ocaml_current Versions.ocaml_411

let to_current =
  let open Migrate_parsetree in
  Versions.migrate Versions.ocaml_411 Versions.ocaml_current

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
    Pparse.parse_interface
      ~tool_name:"gen_js_iface"
      src |> from_current.Migrate_parsetree.Versions.copy_signature
  in
  let str = str_of_sg ~global_attrs:[] sg in
  ignore (check_loc_mapper.Ast_mapper.signature check_loc_mapper sg);
  let str = clear_attr_mapper.Ast_mapper.structure clear_attr_mapper str in
  Format.fprintf (Format.formatter_of_out_channel oc) "%a@." Pprintast.structure (to_current.copy_structure str);
  if !out <> "-" then close_out oc


let mapper =
  { mapper with
    Ast_mapper.structure = (fun _this str ->
        check_loc_mapper.Ast_mapper.structure
          check_loc_mapper
          (mapper.Ast_mapper.structure mapper str));
  }

let mark_attributes_as_used mapper =
  (* mark `js.***` attributes as used in mli. *)
  let attribute : _ -> attribute -> _ =
    fun this ({attr_name = {txt; _}; _} as attr) ->
      if is_js_attribute txt then
        ignore (filter_attr txt attr : bool);
      mapper.Ast_mapper.attribute this attr
  in
  { mapper with attribute }
