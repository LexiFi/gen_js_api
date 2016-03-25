(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

(** OCaml view on JS exceptions *)

type t

val name: t -> string
val message: t -> string
val stack: t -> string option
val to_string: t -> string

exception Error of t
