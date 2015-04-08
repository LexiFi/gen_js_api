(***************************************************************************)
(*  Copyright (C) 2000-2015 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

(* $Id: ojs.mli 80187 2015-03-27 16:40:57Z afrisch $ *)

(** Binding with JS values. *)

type t

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

val option_of_js: (t -> 'a) -> t -> 'a option
(** Both [null] and [undefined] are mapped to [None]. *)
val option_to_js: ('a -> t) -> 'a option -> t
(** [None] is mapped to [null]. *)


val fun_to_js: ('a -> 'b) -> t

val call: t -> string -> t array -> t
val call_unit: t -> string -> t array -> unit

val apply: t -> t array -> t
val apply_unit: t -> t array -> unit

val get: t -> string -> t
val set: t -> string -> t -> unit

val obj: (string * t) array -> t

val array_get: t -> int -> t
val array_set: t -> int -> t -> unit

val variable: string -> t
