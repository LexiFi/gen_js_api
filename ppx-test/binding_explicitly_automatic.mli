(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

[@@@js.implem [@@@warning "-22"]]

module M : sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t

  val prop_get_arg: t -> int [@@js]
  val prop_get: unit -> int [@@js]
  val set_prop: t -> int -> unit [@@js]
  val set_global: int -> unit [@@js]
  val new_thing_unit: unit -> t [@@js]
  val new_thing_args: int -> t [@@js]
  val method_call_global: t -> unit [@@js]
  val method_call_unit: t -> unit -> int [@@js]
  val method_call_args: t -> int -> int [@@js]
  val method_call_unit_unit: t -> unit -> unit [@@js]
  val method_call_args_unit: t -> int -> unit [@@js]
  val global: t [@@js]

  [@@@warning "-32"]
  val get: t -> int -> string option [@@js]
  val set: t -> int -> string -> unit [@@js]
  val get: t -> string -> string option [@@js]
  val set: t -> string -> string -> unit [@@js]
  [@@@warning "+32"]
  val get: t -> Ojs.t -> string option [@@js]
  val set: t -> Ojs.t -> string -> unit [@@js]
end