type timeout_id
val timeout_id_to_js: timeout_id -> Ojs.t
val timeout_id_of_js: Ojs.t -> timeout_id

type interval_id
val interval_id_to_js: interval_id -> Ojs.t
val interval_id_of_js: Ojs.t -> interval_id


val set_interval: (unit -> unit) -> int -> interval_id
val set_timeout: (unit -> unit) -> int -> timeout_id
val clear_timeout: timeout_id -> unit
val clear_interval: interval_id -> unit
