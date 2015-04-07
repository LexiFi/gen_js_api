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

val of_fun: (t -> t) -> t
val of_unit_fun: (unit -> unit) -> t

val call: t -> string -> t array -> t
val call_unit: t -> string -> t array -> unit

val apply: t -> t array -> t
val apply_unit: t -> t array -> unit

val get: t -> string -> t
val set: t -> string -> t -> unit

val obj: (string * t) array -> t

val array_get: t -> int -> t
val array_set: t -> int -> t -> unit

val array_of_js: (t -> 'a) -> t -> 'a array
val array_to_js: ('a -> t) -> 'a array -> t

val option_of_js: (t -> 'a) -> t -> 'a option
    (** Both [null] and [undefined] are mapped to [None]. *)
val option_to_js: ('a -> t) -> 'a option -> t
  (** [None] is mapped to [null]. *)

val variable: string -> t
