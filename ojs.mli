(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by Alain Frisch and LexiFi.                             *)

(** Binding with JS values. *)

type t

val t_of_js: t -> t
val t_to_js: t -> t

val string_of_js: t -> string
val string_to_js: string -> t

val int_of_js: t -> int
val int_to_js: int -> t

val bool_of_js: t -> bool
val bool_to_js: bool -> t

val float_of_js: t -> float
val float_to_js: float -> t

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

val fun_to_js: (t -> 'a) -> t
val fun_unit_to_js: (unit -> 'a) -> t
val fun_to_js_args: (t -> 'a) -> t

val call: t -> string -> t array -> t
val call_unit: t -> string -> t array -> unit

val apply: t -> t array -> t
val apply_unit: t -> t array -> unit

val get: t -> string -> t
val set: t -> string -> t -> unit

val obj: (string * t) array -> t

val array_make: int -> t
val array_get: t -> int -> t
val array_set: t -> int -> t -> unit

val variable: string -> t

val new_obj: string -> t array -> t

val type_of: t -> string

class obj: t ->
  object
    method to_js: t
  end
