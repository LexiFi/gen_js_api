(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

open Ppxlib

val check_attribute : bool ref

val mapper : Ast_traverse.map

val module_expr_rewriter: loc:Location.t -> attrs:Ppxlib.Parsetree.attributes -> Ppxlib.Parsetree.signature -> Ppxlib.module_expr

val js_of_rewriter: loc:Location.t -> core_type -> expression

val js_to_rewriter: loc:Location.t -> core_type -> expression

val type_decl_rewriter: loc:Location.t -> rec_flag -> type_declaration list -> structure

val mark_attributes_as_used: Ast_traverse.map

val standalone : unit -> unit
