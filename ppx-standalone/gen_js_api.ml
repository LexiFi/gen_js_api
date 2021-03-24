(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

open Ppxlib

let () =
  try
    Gen_js_api_ppx.standalone ()
  with exn ->
    Format.eprintf "%a@." Location.report_exception exn;
    exit 2
