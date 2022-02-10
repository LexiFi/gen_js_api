(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

type t = Jsoo_runtime.Error.t

external coerce : t -> Ojs.t = "%identity"
let name x = Ojs.string_of_js (Ojs.get_prop_ascii (coerce x) "name")
let message x = Ojs.string_of_js (Ojs.get_prop_ascii (coerce x) "message")
let stack x = Ojs.option_of_js Ojs.string_of_js (Ojs.get_prop_ascii (coerce x) "stack")
let to_string x = Ojs.string_of_js (Ojs.call (coerce x) "toString" [||])

exception Error = Jsoo_runtime.Error.Exn

let () =
  Printexc.register_printer (function
      | Error x -> Some (to_string x)
      | _ -> None
    )
