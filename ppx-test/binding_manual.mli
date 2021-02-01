(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)
module M : sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t

  val prop_get_arg: t -> int [@@js.get "propGetArg"]
  val prop_get: unit -> int [@@js.get "propGet"]
  val set_prop: t -> int -> unit [@@js.set "prop"]
  val set_global: int -> unit [@@js.set "global"]
  val new_thing_unit: unit -> t [@@js.new "ThingUnit"]
  val new_thing_args: int -> t [@@js.new "ThingArgs"]
  val method_call_global: t -> unit [@@js.call "methodCallGlobal"]
  val method_call_unit: t -> unit -> int [@@js.call "methodCallUnit"]
  val method_call_args: t -> int -> int[@@js.call "methodCallArgs"]
  val method_call_unit_unit: t -> unit -> unit[@@js.call "methodCallUnitUnit"]
  val method_call_args_unit: t -> int -> unit[@@js.call "methodCallArgsUnit"]
  val global: t[@@js.global "global"]

  [@@@warning "-32"]
  val get: t -> int -> string option [@@js.index_get]
  val set: t -> int -> string -> unit [@@js.index_set]
  val get: t -> string -> string option [@@js.index_get]
  val set: t -> string -> string -> unit [@@js.index_set]
  [@@@warning "+32"]
  val get: t -> Ojs.t -> string option [@@js.index_get]
  val set: t -> Ojs.t -> string -> unit [@@js.index_set]
end