(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

(** Binding with JS values. *)

type t
(** The universal type representing arbitrary JS values. *)

(** {2 Mapper for built-in types} *)

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


(** {2 Wrap OCaml functions as JS functions} *)

external fun_to_js: int -> (t -> 'a) -> t = "caml_js_wrap_callback_strict"
(** Wrap an OCaml function of known arity (>=1) into a JS function.
    Extra arguments are discarded and missing argument are filled with
    'undefined'.
*)

external fun_to_js_args: (t -> 'a) -> t = "caml_ojs_wrap_fun_arguments"
(** Wrap an OCaml function taking JS arguments as a JS array. *)


(** {2 JS objects} *)

external get: t -> string -> t = "caml_js_get"

external set: t -> string -> t -> unit = "caml_js_set"

external obj: (string * t) array -> t = "caml_js_object"

val empty_obj: unit -> t

val has_property: t -> string -> bool
external iter_properties: t -> (string -> unit) -> unit = "caml_ojs_iterate_properties"

(** {2 Calling JS functions} *)

external call: t -> string -> t array -> t = "caml_js_meth_call"
(** Call a method on an object (binding 'this' to the object). *)

external apply: t -> t array -> t = "caml_js_fun_call"
(** Call a function. *)

external new_obj: t -> t array -> t = "caml_js_new"
(** Call a constructor *)

val call_arr: t -> string -> t -> t
(** Variant of [Ojs.call] where the arguments are passed as an already
    built JS array. *)

val apply_arr: t -> t -> t
(** Variant of [Ojs.apply] where the arguments are passed as an already
    built JS array. *)

external new_obj_arr: t -> t -> t = "caml_ojs_new_arr"
(** Variant of [Ojs.new_obj] where the arguments are passed as an already
    built JS array. *)


(** {2 Arrays} *)

val array_make: int -> t
val array_get: t -> int -> t
val array_set: t -> int -> t -> unit


(** {2 Misc} *)

val global: t
val null: t

external variable: string -> t = "caml_js_var"

val type_of: t -> string

class obj: t ->
  object
    method to_js: t
  end

external delete: t -> string -> unit = "caml_js_delete"

val is_null: t -> bool

val obj_type: t -> string
(** Returns:
    "[object Array]"
    "[object Object]"
    "[object Number]"
    "[object String]"
    "[object Null]"
    "[object Boolean]"
*)
