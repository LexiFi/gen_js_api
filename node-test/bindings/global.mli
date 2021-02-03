type timeout_id [@@js]
type interval_id [@@js]

val set_interval: (unit -> unit) -> int -> interval_id
val set_timeout: (unit -> unit) -> int -> timeout_id
val clear_timeout: timeout_id -> unit
val clear_interval: interval_id -> unit
