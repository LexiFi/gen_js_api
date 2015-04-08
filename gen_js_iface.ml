(***************************************************************************)
(*  Copyright (C) 2000-2015 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

(* $Id: gen_js_iface.ml 80189 2015-03-27 17:18:29Z afrisch $ *)

(* TODO:

   - Support for sum types, mapped either to integers or strings (for
     constant constructors) or to records with a discrimator field.
     (To be customized by attributes.)

   - Also support running the tool as a ppx working on .ml files
     to generate *_of_js, *_to_js functions associated to type
     definitions (marked with an attribute), to generate bindings
     for "val" declarations, and also to support typed-index mapping
     betwen values  [%%to_js: tyexpr],  [%of_js: tyexpr] (returning
     functions).
*)



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

exception Error of Location.t * error

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

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) -> Some (Location.error_of_printer loc print_error err)
      | _ -> None
    )


(** AST *)

type typ =
  | Arrow of typ list * typ
  | Unit
  | Js
  | Name of string * typ list

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

type decl =
  | Module of string * decl list
  | Type of rec_flag * Parsetree.type_declaration list
  | Val of string * typ * valdef * Location.t

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
      | "unit", [] -> Unit
      | "Ojs.t", [] -> Js
      | s, tl -> Name (s, List.map parse_typ tl)
      end
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

let auto loc s ty =
  match ty with
  | Arrow ([Name (t, [])], Js) when check_suffix ~suffix:"_to_js" s = Some t ->
      Cast

  | Arrow ([Js], Name (t, [])) when check_suffix ~suffix:"_of_js" s = Some t ->
      Cast

  | Arrow ([Name _], _) ->
      PropGet s

  | Arrow ([Name _; _], Unit) when has_prefix ~prefix:"set_" s ->
      PropSet (drop_prefix ~prefix:"set_" s)


  | Arrow (Name _ :: _, _) ->
      MethCall s

  | _ ->
      Global s

let id_of_expr = function
  | {pexp_desc=Pexp_constant (Const_string (s, _)); _}
  | {pexp_desc=Pexp_ident {txt=Lident s;_}; _}
  | {pexp_desc=Pexp_construct ({txt=Lident s;_}, None); _} -> s
  | e -> error e.pexp_loc Identifier_expected

let expr_of_stritem = function
  | {pstr_desc=Pstr_eval (e, _); _} -> e
  | p -> error p.pstr_loc Expression_expected

let expr_of_payload loc = function
  | PStr [x] -> expr_of_stritem x
  | _ -> error loc Expression_expected

let parse_valdecl loc s ty attrs =
  let parse_attr defs (k, v) =
    let opt_name () =
      match v with
      | PStr [] -> s (* default *)
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
        let s =
          match v with
          | PStr [] ->
              begin match check_prefix ~prefix:"set_" s with
              | None -> error loc Setter_name
              | Some s2 -> s2
              end
          | e -> opt_name ()
        in
        PropSet s :: defs
    | "js.meth" ->
        MethCall (opt_name ()) :: defs
    | "js.global" ->
        Global (opt_name ()) :: defs
    | _ ->
        defs
  in
  let defs = List.fold_left parse_attr [] attrs in
  match defs with
  | [x] -> x
  | [] -> auto loc s ty
  | _ ->
      error loc Multiple_binding_declarations

let rec parse_sig_item s =
  match s.psig_desc with
  | Psig_value {pval_prim = []; pval_name; pval_type; pval_attributes; pval_loc; _} ->
      let name = pval_name.txt in
      let ty = parse_typ pval_type in
      Val (name, ty,
           parse_valdecl pval_loc name ty pval_attributes,
           pval_loc
          )
  | Psig_type (rec_flag, decls) ->
      Type (rec_flag, decls)
  | Psig_module {pmd_name; pmd_type = {pmty_desc = Pmty_signature si; _}; _} ->
      Module (pmd_name.txt, parse_sig si)
  | _ ->
      error s.psig_loc Cannot_parse_sigitem

and parse_sig s =
  List.map parse_sig_item s

(** Code generation *)

let var x = Exp.ident (mknoloc (Longident.parse x))
let app f args = Exp.apply f (List.map (fun e -> (Nolabel, e)) args)
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

let func = List.fold_right (fun s rest -> fun_ s rest)
let func_or_unit args body =
  match args with
  | [] -> fun_unit body
  | args -> func args body

let app_or_unit f args =
  match args with
  | [] -> app f [Exp.construct (mknoloc (Lident "()")) None]
  | args -> app f args

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

let rec js2ml ty exp =
  match ty with
  | Js ->
      exp
  | Name (s, tl) ->
      let s = if builtin_type s then "Ojs." ^ s else s in
      let args = List.map js2ml_fun tl in
      app (Exp.ident (mknoloc (Longident.parse (s ^ "_of_js")))) (args @ [exp])
  | Arrow (ty_args, ty_res) ->
      let ty_args = map_args ty_args in
      let args = gen_args ty_args in
      let res =
        ojs (if ty_res=Unit then "apply_unit" else "apply")
          [
            exp;
            Exp.array (List.map snd args)
          ]
      in
      let res = map_res res ty_res in
      func_or_unit (List.map fst args) res

  | Unit ->
      assert false
      (* Exp.extension (mknoloc "unknown", PStr []) *)

and ml2js ty exp =
  match ty with
  | Js ->
      exp
  | Name (s, tl) ->
      let s = if builtin_type s then "Ojs." ^ s else s in
      let args = List.map ml2js_fun tl in
      app (Exp.ident (mknoloc (Longident.parse (s ^ "_to_js")))) (args @ [exp])
  | Arrow (ty_args, ty_res) ->
      let ty_args = map_args ty_args in
      let args = gen_args ~map:js2ml ty_args in
      let res = app_or_unit exp (List.map snd args) in
      let f = func_or_unit (List.map fst args) (map_res ~map:ml2js res ty_res) in
      ojs "fun_to_js" [f]

  | Unit ->
      assert false
      (* Exp.extension (mknoloc "unknown", PStr []) *)

and js2ml_fun ty = mkfun (js2ml ty)
and ml2js_fun ty = mkfun (ml2js ty)


and gen_args ?(name = fun _ -> fresh ()) ?(map = ml2js) ty_args =
  List.mapi
    (fun i ty ->
       let s = name i in
       s, map ty (var s)
    )
    ty_args

and map_res ?(map=js2ml) res ty_res =
  match ty_res with
  | Unit -> res
  | _ -> map ty_res res

and map_args = function
  | [Unit] -> []
  | args -> args



and gen_typ = function
  | Name (s, tyl) ->
      Typ.constr (mknoloc (Longident.parse s)) (List.map gen_typ tyl)
  | Js ->
      Typ.constr (mknoloc (Longident.parse "Ojs.t")) []
  | Unit ->
      Typ.constr (mknoloc (Lident "unit")) []
  | Arrow (tl, t2) ->
      List.fold_right
        (fun t1 t2 -> Typ.arrow Nolabel (gen_typ t1) t2)
        tl
        (gen_typ t2)

let rec gen_decls env si =
  List.concat (List.map (gen_decl env) si)

and gen_funs_record lbls =
  let prepare_label l =
    let js = ref l.pld_name.txt in
    List.iter
      (fun (k, v) ->
         match k.txt with
         | "js" ->
             js := id_of_expr (expr_of_payload k.loc v)
         | _ ->
             ()
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

and gen_funs p =
  let name = p.ptype_name.txt in
  let of_js, to_js =
    match p.ptype_manifest, p.ptype_kind with
    | Some ty, Ptype_abstract ->
        let ty = parse_typ ty in
        js2ml_fun ty, ml2js_fun ty
    | _, Ptype_record lbls ->
        gen_funs_record lbls
    | _ ->
        error p.ptype_loc Cannot_parse_type
  in
  [
    Vb.mk
      (Pat.constraint_
         (Pat.var (mknoloc (name ^ "_of_js")))
         (gen_typ (Arrow ([Js], Name (name, [])))))
      of_js;
    Vb.mk
      (Pat.constraint_
         (Pat.var (mknoloc (name ^ "_to_js")))
         (gen_typ (Arrow ([Name (name, [])], Js))))
      to_js
  ]


and gen_decl env = function
  | Type (rec_flag, decls) ->
      let decls = List.map (fun t -> {t with ptype_private = Public}) decls in
      let funs = List.concat (List.map gen_funs decls) in
      [ Str.type_ rec_flag decls; Str.value rec_flag funs ]
  | Module (s, decls) ->
      [ Str.module_ (Mb.mk (mknoloc s) (Mod.structure (gen_decls env decls))) ]

  | Val (s, ty, decl, loc) ->
      let d = gen_def loc decl ty in
      [ def s (gen_typ ty) d ]

and gen_def loc decl ty =
  match decl, ty with
  | Cast, Arrow ([ty_arg], ty_res) ->
      mkfun (fun this -> js2ml ty_res (ml2js ty_arg this))

  | PropGet s, Arrow ([ty_this], ty_res) ->
      mkfun (fun this -> js2ml ty_res (ojs "get" [ml2js ty_this this; str s]))

  | PropSet s, Arrow ([Name _ as ty_this; ty_arg], Unit) ->
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
      let ty_args = map_args ty_args in
      let args = gen_args ty_args in
      let res this =
        ojs
          (if ty_res = Unit then "call_unit" else "call")
          [
            ml2js ty_this this;
            str s;
            Exp.array (List.map snd args)
          ]
      in
      mkfun
        (fun this ->
           func_or_unit (List.map fst args) (map_res (res this) ty_res)
        )

  | Global s, ty_res ->
      let res = ojs "variable" [str s] in
      js2ml ty_res res

  | Expr e, Arrow (ty_args, ty_res) ->
      let ty_args = map_args ty_args in
      let args = gen_args ~name:(Printf.sprintf "arg%i") ty_args in
      func_or_unit (List.map fst args) (map_res (gen_expr args e) ty_res)

  | Expr e, ty ->
      js2ml ty (gen_expr [] e)

  | _ ->
      error loc Binding_type_mismatch

and gen_expr ids = function
  | Call (Id "call", obj :: Str meth :: args) ->
      ojs "call"
        [
          gen_expr ids obj;
          str meth;
          Exp.array (List.map (gen_expr ids) args)
        ]
  | Call (Id "global", [Str s]) ->
      ojs "variable" [str s]
  | Str s ->
      ml2js (Name ("string", [])) (str s)
  | Id s when List.mem_assoc s ids ->
      List.assoc s ids
  | _ ->
      assert false
(*
      Exp.extension (mknoloc "unknown", PStr [])
*)

(** Main *)


let main () =
  let ocaml_ast =
    Pparse.parse_interface Format.err_formatter
      ~tool_name:"gen_js_iface"
      Sys.argv.(1)
  in
  let decls = parse_sig ocaml_ast in
  let str_ast = gen_decls () decls in
  Format.printf "%a@." Pprintast.structure str_ast


let () =
  try main ()
  with exn ->
    Format.eprintf "%a@." Location.report_exception exn;
    exit 2
