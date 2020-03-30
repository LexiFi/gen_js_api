(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

val mark_as_handled_manually : (Migrate_parsetree.Ast_408.Parsetree.attribute -> unit) ref

val check_attribute : bool ref

val mapper : Migrate_parsetree.Ast_408.Ast_mapper.mapper

val mark_attributes_as_used
  :  Migrate_parsetree.Ast_408.Ast_mapper.mapper
  -> Migrate_parsetree.Ast_408.Ast_mapper.mapper

val standalone : unit -> unit
