(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

(** Binding with JS values. *)

type t = Jsoo_runtime.Js.t
(** The universal type representing arbitrary JS values. *)

(** {2 Constants } *)

val null: t

val undefined: t

val global: t

(** {2 Mapper for built-in types} *)

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

val unit_of_js: t -> unit
val unit_to_js: unit -> t

class obj: t ->
  object
    method to_js: t
  end

module type T =
  sig
    type js := t
    type t
    val t_to_js : t -> js
    val t_of_js : js -> t
  end

(* Ojs.T instances for built-in types *)
module Int : T with type t = int
module String : T with type t = string
module Bool : T with type t = bool
module Float : T with type t = float
module Array (A: T) : T with type t = A.t array
module List (A: T) : T with type t = A.t list
module Option (A: T) : T with type t = A.t option

(** {2 Deprecated Functions } *)

val fun_to_js: int -> (t -> 'a) -> t
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.callback_with_arity instead."]

val fun_to_js_args: (t -> 'a) -> t
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.callback_with_arguments instead."]

val get: t -> string -> t
  [@@ocaml.deprecated "Use Ojs.get_prop_ascii instead."]

val set: t -> string -> t -> unit
  [@@ocaml.deprecated "Use Ojs.set_prop_ascii instead."]

val delete: t -> string -> unit
  [@@ocaml.deprecated "Use Ojs.delete_prop_ascii instead."]

val get_prop_ascii: t -> string -> t
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.get instead."]
  (** Get the property from an object (only works if the property key is a plain ascii string). *)

val set_prop_ascii: t -> string -> t -> unit
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.set instead."]
  (** Set an object property (only works if the property key is a plain ascii string). *)

val delete_prop_ascii: t -> string -> unit
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.delete instead."]
  (** Delete an object property (only works if the property key is a plain ascii string). *)

val get_prop: t -> t -> t
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.get instead."]
  (** Get the property from an object. *)

val set_prop: t -> t -> t -> unit
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.set instead."]
  (** Set an object property. *)

val delete_prop: t -> t -> unit
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.delete instead."]
  (** Delete an object property. *)

val obj: (string * t) array -> t
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.obj instead."]

val empty_obj: unit -> t
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.obj instead."]

val has_property: t -> string -> bool
  [@@ocaml.deprecated "Please do not use. It will be removed."]

val iter_properties: t -> (string -> unit) -> unit
  [@@ocaml.deprecated "Please do not use. It will be removed."]

val call: t -> string -> t array -> t
  [@@ocaml.deprecated  "Use Jsoo_runtime.Js.meth_call instead."]

val apply: t -> t array -> t
  [@@ocaml.deprecated  "Use Jsoo_runtime.Js.fun_call instead."]

val new_obj: t -> t array -> t
  [@@ocaml.deprecated  "Use Jsoo_runtime.Js.new_obj instead."]

val call_arr: t -> string -> t -> t
  [@@ocaml.deprecated  "Use Jsoo_runtime.Js.meth_call instead."]

val apply_arr: t -> t -> t
  [@@ocaml.deprecated  "Use Jsoo_runtime.Js.fun_call instead."]

val new_obj_arr: t -> t -> t
  [@@ocaml.deprecated  "Use Jsoo_runtime.Js.new_obj_arr."]

val array_make: int -> t
  [@@ocaml.deprecated  "Use another binding of the global Array module."]

val array_get: t -> int -> t
  [@@ocaml.deprecated  "Use another binding of the global Array module."]

val array_set: t -> int -> t -> unit
  [@@ocaml.deprecated  "Use another binding of the global Array module."]

val variable: string -> t
  [@@ocaml.deprecated "For compatibility only."]

val type_of: t -> string
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.typeof instead."]

val instance_of: t -> constr:t -> bool
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.instanceof instead."]

val is_null: t -> bool
  [@@ocaml.deprecated "Please do not use. It will be removed."]

val obj_type: t -> string
  [@@ocaml.deprecated "Please do not use. It will be removed."]
