[@@@js.scope "Buffer"]

type t = private Ojs.t
val t_of_js: Ojs.t -> t
val t_to_js: t -> Ojs.t

[@@@js.static]
val alloc: int -> t
val from: string -> t
val concat: t list -> t

[@@@js.prototype]
val length: t -> int
val get: t -> int -> int option[@@js.custom
  let get buf index = [%js.to:int option](Ojs.array_get buf index)
    ]
val set: t -> int -> int -> unit[@@js.custom
  let set buf index value =
    Ojs.array_set buf index ([%js.of:int] value)
  ]
val write: t -> string -> unit
val slice: t -> int -> int -> t
val to_string: t -> string
val copy: t -> dst:t -> start:int -> dst_start:int -> dst_end:int -> int