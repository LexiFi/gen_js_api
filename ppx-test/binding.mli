(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)
module M : sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t

  val cast: t -> string [@@js.cast]

  val prop_get_arg: t -> int [@@js.get "getter"]
  val prop_get: unit -> int [@@js.get "getter"]

  val global: t [@@js.global "global"]
  val global_arrow: int -> int [@@js.global "global"]

  val prop_set: t -> int -> unit [@@js.set "setter"]
  val prop_set_global: t -> unit [@@js.set "setter"]

  val method_call_global: t -> int [@@js.call "method"]
  val method_call_global_unit: t -> unit [@@js.call "method"]
  val method_call_unit: t -> unit -> int [@@js.call "method"]
  val method_call_args: t -> int -> int [@@js.call "method"]
  val method_call_unit_unit: t -> unit -> unit [@@js.call "method"]
  val method_call_args_unit: t -> int -> unit [@@js.call "method"]

  val new_thing: int -> t [@@js.new]

  val builder: ?x:int -> (int [@js "y"]) -> z:int -> t [@@js.builder]

  val index_get_int: t -> int -> string option [@@js.index_get]
  val index_get_string: t -> string -> string option [@@js.index_get]
  val index_get_generic: t -> Ojs.t -> string option [@@js.index_get]

  val index_set_int: t -> int -> string -> unit [@@js.index_set]
  val index_set_string: t -> string -> string -> unit [@@js.index_set]
  val index_set_generic: t -> Ojs.t -> string -> unit [@@js.index_set]
end