(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

(** JS dates *)

(** {2 Type definitions} *)

type t = private Ojs.t
val t_of_js: Ojs.t -> t
val t_to_js: t -> Ojs.t

val now: unit -> t [@@js.new "Date"]
val from_milliseconds: float -> t [@@js.new "Date"]
val from_string: string -> t [@@js.new "Date"]

val create: year:int -> month:int -> ?day:(int [@js.default 1]) -> ?hours:(int [@js.default 0]) -> ?minutes:(int [@js.default 0]) -> ?seconds:(int [@js.default 0]) -> ?ms:(int [@js.default 0]) -> unit -> t [@@js.new "Date"]

val get_UTC_date: t -> int [@@js.call]
val get_UTC_day: t -> int [@@js.call]
val get_UTC_full_year: t -> int [@@js.call]
val get_UTC_hours: t -> int [@@js.call]
val get_UTC_milliseconds: t -> int [@@js.call]
val get_UTC_minutes: t -> int [@@js.call]
val get_UTC_month: t -> int [@@js.call]
val get_UTC_seconds: t -> int [@@js.call]

val set_UTC_date: t -> int -> unit [@@js.call]
val set_UTC_full_year: t -> int -> unit [@@js.call]
val set_UTC_hours: t -> int -> unit [@@js.call]
val set_UTC_milliseconds: t -> int -> unit [@@js.call]
val set_UTC_minutes: t -> int -> unit [@@js.call]
val set_UTC_month: t -> int -> unit [@@js.call]
val set_UTC_seconds: t -> int -> unit [@@js.call]

val get_date: t -> int [@@js.call]
val get_day: t -> int [@@js.call]
val get_full_year: t -> int [@@js.call]
val get_hours: t -> int [@@js.call]
val get_milliseconds: t -> int [@@js.call]
val get_minutes: t -> int [@@js.call]
val get_month: t -> int [@@js.call]
val get_seconds: t -> int [@@js.call]

val set_date: t -> int -> unit [@@js.call]
val set_full_year: t -> int -> unit [@@js.call]
val set_hours: t -> int -> unit [@@js.call]
val set_milliseconds: t -> int -> unit [@@js.call]
val set_minutes: t -> int -> unit [@@js.call]
val set_month: t -> int -> unit [@@js.call]
val set_seconds: t -> int -> unit [@@js.call]

val get_time: t -> float [@@js.call]
val set_time: t -> float -> unit [@@js.call]

val get_timezone_offset: t -> int [@@js.call]

val to_locale_date_string: t -> string [@@js.call]
val to_locale_string: t -> string [@@js.call]
val to_locale_time_string: t -> string [@@js.call]

val to_date_string: t -> string [@@js.call]
val to_time_string: t -> string [@@js.call]

val to_UTC_string: t -> string [@@js.call]

val to_string: t -> string [@@js.call]
