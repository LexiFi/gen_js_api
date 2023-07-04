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

external fun_to_js: int -> (t -> 'a) -> t = "caml_js_wrap_callback_strict"
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.callback_with_arity instead."]

external fun_to_js_args: (t -> 'a) -> t = "caml_js_wrap_callback_arguments"
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.callback_with_arguments instead."]

external get: t -> string -> t = "caml_js_get"
  [@@ocaml.deprecated "Use Ojs.get_prop_ascii instead."]

external set: t -> string -> t -> unit = "caml_js_set"
  [@@ocaml.deprecated "Use Ojs.set_prop_ascii instead."]

external delete: t -> string -> unit = "caml_js_delete"
  [@@ocaml.deprecated "Use Ojs.delete_prop_ascii instead."]

external get_prop_ascii: t -> string -> t = "caml_js_get"
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.get instead."]
  (** Get the property from an object (only works if the property key is a plain ascii string). *)

external set_prop_ascii: t -> string -> t -> unit = "caml_js_set"
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.set instead."]
  (** Set an object property (only works if the property key is a plain ascii string). *)

external delete_prop_ascii: t -> string -> unit = "caml_js_delete"
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.delete instead."]
  (** Delete an object property (only works if the property key is a plain ascii string). *)

external get_prop: t -> t -> t = "caml_js_get"
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.get instead."]
  (** Get the property from an object. *)

external set_prop: t -> t -> t -> unit = "caml_js_set"
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.set instead."]
  (** Set an object property. *)

external delete_prop: t -> t -> unit = "caml_js_delete"
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.delete instead."]
  (** Delete an object property. *)

external obj: (string * t) array -> t = "caml_js_object"
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.obj instead."]

val empty_obj: unit -> t
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.obj instead."]

val has_property: t -> string -> bool
  [@@ocaml.deprecated "Please do not use. It will be removed."]

val iter_properties: t -> (string -> unit) -> unit
  [@@ocaml.deprecated "Please do not use. It will be removed."]

external call: t -> string -> t array -> t = "caml_js_meth_call"
  [@@ocaml.deprecated  "Use Jsoo_runtime.Js.meth_call instead."]

external apply: t -> t array -> t = "caml_js_fun_call"
  [@@ocaml.deprecated  "Use Jsoo_runtime.Js.fun_call instead."]

external new_obj: t -> t array -> t = "caml_js_new"
  [@@ocaml.deprecated  "Use Jsoo_runtime.Js.new_obj instead."]

val call_arr: t -> string -> t -> t
  [@@ocaml.deprecated  "Use Jsoo_runtime.Js.meth_call instead."]

val apply_arr: t -> t -> t
  [@@ocaml.deprecated  "Use Jsoo_runtime.Js.fun_call instead."]

external new_obj_arr: t -> t -> t = "caml_ojs_new_arr"
  [@@ocaml.deprecated  "Use Jsoo_runtime.Js.new_obj_arr."]

val array_make: int -> t
  [@@ocaml.deprecated  "Use another binding of the global Array module."]

val array_get: t -> int -> t
  [@@ocaml.deprecated  "Use another binding of the global Array module."]

val array_set: t -> int -> t -> unit
  [@@ocaml.deprecated  "Use another binding of the global Array module."]

external variable: string -> t = "caml_js_var"
  [@@ocaml.deprecated "For compatibility only."]

val type_of: t -> string
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.typeof instead."]

val instance_of: t -> constr:t -> bool
  [@@ocaml.deprecated "Use Jsoo_runtime.Js.instanceof instead."]

val is_null: t -> bool
  [@@ocaml.deprecated "Please do not use. It will be removed."]

val obj_type: t -> string
  [@@ocaml.deprecated "Please do not use. It will be removed."]
