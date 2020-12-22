[@@@js.scope "Buffer"]

type t = private Ojs.t
val t_of_js: Ojs.t -> t
val t_to_js: t -> Ojs.t

val alloc: int -> t[@@js.global]
val from: string -> t[@@js.global]
val concat: t list -> t[@@js.global]

val length: t -> int [@@js.get]
val get: t -> int -> int option[@@js.custom
  let get buf index = [%js.to:int option](Ojs.array_get buf index)
    ]
val set: t -> int -> int -> unit[@@js.custom
  let set buf index value =
    Ojs.array_set buf index ([%js.of:int] value)
  ]
val write: t -> string -> int[@@js.call]
val slice: t -> int -> int -> t[@@js.call]
val to_string: t -> string[@@js.call]
val copy: t -> dst:t -> start:int -> dst_start:int -> dst_end:int -> int[@@js.call]
