(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

(** Binding with JS values. *)

type t

external t_of_js: t -> t = "%identity"
external t_to_js: t -> t = "%identity"

external string_of_js: t -> string = "caml_js_to_string"
external string_to_js: string -> t = "caml_js_from_string"

external int_of_js: t -> int = "%identity"
external int_to_js: int -> t = "%identity"

external bool_of_js: t -> bool = "caml_js_to_bool"
external bool_to_js: bool -> t = "caml_js_from_bool"

external float_of_js: t -> float = "%identity"
external float_to_js: float -> t = "%identity"

val array_of_js: (t -> 'a) -> t -> 'a array
val array_to_js: ('a -> t) -> 'a array -> t

val list_of_js: (t -> 'a) -> t -> 'a list
val list_to_js: ('a -> t) -> 'a list -> t

val array_of_js_from: (t -> 'a) -> t -> int -> 'a array
val list_of_js_from: (t -> 'a) -> t -> int -> 'a list

val option_of_js: (t -> 'a) -> t -> 'a option
(** Both [null] and [undefined] are mapped to [None]. *)
val option_to_js: ('a -> t) -> 'a option -> t
(** [None] is mapped to [null]. *)

external fun_to_js: int -> (t -> 'a) -> t = "caml_js_wrap_callback_strict"
(** Wrap an OCaml function of known arity (>=1) into a JS function.
    Extra arguments are discarded and missing argument are filled with
    'undefined'.
*)

external fun_to_js_args: (t -> 'a) -> t = "caml_ojs_wrap_fun_arguments"

external call: t -> string -> t array -> t = "caml_js_meth_call"
external apply: t -> t array -> t = "caml_js_fun_call"

external get: t -> string -> t = "caml_js_get"
external set: t -> string -> t -> unit = "caml_js_set"

external obj: (string * t) array -> t = "caml_js_object"

val array_make: int -> t
val array_get: t -> int -> t
val array_set: t -> int -> t -> unit

external variable: string -> t = "caml_js_var"

val new_obj: string -> t array -> t

val type_of: t -> string

class obj: t ->
  object
    method to_js: t
  end

external iterate_properties: t -> (string -> unit) -> unit = "caml_ojs_iterate_properties"
