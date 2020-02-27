(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

type t = Ojs.t

let name x = Ojs.string_of_js (Ojs.get x "name")
let message x = Ojs.string_of_js (Ojs.get x "message")
let stack x = Ojs.option_of_js Ojs.string_of_js (Ojs.get x "stack")
let to_string x = Ojs.string_of_js (Ojs.call x "toString" [||])

exception Error of t

let () = Callback.register_exception "jsError" (Error (Ojs.obj [||]))

(* The js_of_ocaml runtime expects to have this registered.
   So it's probably a bad idea to use both this Ojs_exn module
   and the js_of_ocaml standard library. *)

let () =
  Printexc.register_printer (function
      | Error x -> Some (to_string x)
      | _ -> None
    )
