(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

open Migrate_parsetree.Ast_408

let () =
  try
    if Array.length Sys.argv < 4 || Sys.argv.(1) <> "-ppx" then Gen_js_api_ppx.standalone ()
    else
      Ast_mapper.run_main (fun _ -> Gen_js_api_ppx.mapper)
  with exn ->
    Format.eprintf "%a@." Location.report_exception exn;
    exit 2
