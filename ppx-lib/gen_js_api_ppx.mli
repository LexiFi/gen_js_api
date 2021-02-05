(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

val mark_as_handled_manually : (Migrate_parsetree.Ast_411.Parsetree.attribute -> unit) ref

val check_attribute : bool ref

val mapper : Migrate_parsetree.Ast_411.Ast_mapper.mapper

val module_expr_rewriter
  :  loc:Location.t
  -> attrs:Migrate_parsetree.Ast_411.Parsetree.attributes
  -> Migrate_parsetree.Ast_411.Parsetree.signature
  -> Migrate_parsetree.Ast_411.Parsetree.module_expr

val js_of_rewriter
  :  loc:Location.t
  -> Migrate_parsetree.Ast_411.Parsetree.core_type
  -> Migrate_parsetree.Ast_411.Parsetree.expression

val js_to_rewriter
  :  loc:Location.t
  -> Migrate_parsetree.Ast_411.Parsetree.core_type
  -> Migrate_parsetree.Ast_411.Parsetree.expression

val type_decl_rewriter
  :  loc:Location.t
  -> Migrate_parsetree.Ast_411.Asttypes.rec_flag
  -> Migrate_parsetree.Ast_411.Parsetree.type_declaration list
  -> Migrate_parsetree.Ast_411.Parsetree.structure

val type_decl_sig_rewriter
  :  loc:Location.t
  -> Migrate_parsetree.Ast_411.Parsetree.type_declaration list
  -> Migrate_parsetree.Ast_411.Parsetree.signature

val mark_attributes_as_used
  :  Migrate_parsetree.Ast_411.Ast_mapper.mapper
  -> Migrate_parsetree.Ast_411.Ast_mapper.mapper

val standalone : unit -> unit
