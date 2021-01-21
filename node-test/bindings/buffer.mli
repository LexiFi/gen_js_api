[@@@js.scope "Buffer"]

type t = private Ojs.t
val t_of_js: Ojs.t -> t
val t_to_js: t -> Ojs.t

val alloc: int -> t[@@js.global]
val from: string -> t[@@js.global]
val concat: t list -> t[@@js.global]

val length: t -> int [@@js.get]
val get: t -> int -> int option [@@js.index_get]
val set: t -> int -> int -> unit[@@js.index_set]
val write: t -> string -> int[@@js.call]
val slice: t -> int -> int -> t[@@js.call]
val to_string: t -> string[@@js.call]
val copy: t -> dst:t -> start:int -> dst_start:int -> dst_end:int -> int[@@js.call]
