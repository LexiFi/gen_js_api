(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

[@@@js.implem [@@@warning "-22"]]

module M : sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t

  val prop_get_arg: t -> int
  val prop_get: unit -> int
  val set_prop: t -> int -> unit
  val set_global: int -> unit
  val new_thing_unit: unit -> t
  val new_thing_args: int -> t
  val method_call_global: t -> unit
  val method_call_unit: t -> unit -> int
  val method_call_args: t -> int -> int
  val method_call_unit_unit: t -> unit -> unit
  val method_call_args_unit: t -> int -> unit
  val global: t

  [@@@warning "-32"]
  val get: t -> int -> string option
  val set: t -> int -> string -> unit
  val get: t -> string -> string option
  val set: t -> string -> string -> unit
  [@@@warning "+32"]
  val get: t -> Ojs.t -> string option
  val set: t -> Ojs.t -> string -> unit
end
