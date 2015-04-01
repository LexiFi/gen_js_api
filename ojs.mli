(***************************************************************************)
(*  Copyright (C) 2000-2015 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

(* $Id: ojs.mli 80187 2015-03-27 16:40:57Z afrisch $ *)

(** Binding with JS values. *)

type t

val to_string: t -> string
val of_string: string -> t

val to_int: t -> int
val of_int: int -> t

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
val to_array: (t -> 'a) -> t -> 'a array
val of_array: ('a -> t) -> 'a array -> t

val variable: string -> t
