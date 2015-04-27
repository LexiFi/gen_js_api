(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

(** JS string and regexp objects *)

(** {2 Type definitions} *)

type t = private Ojs.t
val t_of_js: Ojs.t -> t
val t_to_js: t -> Ojs.t

type regexp = private Ojs.t
val regexp_of_js: Ojs.t -> regexp
val regexp_to_js: regexp -> Ojs.t

(** {2 Conversion between JS strings and OCaml string} *)

val to_string: t -> string [@@js.cast]
val of_string: string -> t [@@js.cast]

(** {2 JS strings} *)

val from_char_code: (int list [@js.variadic]) -> t
    [@@js.global "String.fromCharCode"]

val char_at: t -> int -> t
val char_code_at: t -> int -> int
val concat: t -> (t list [@js.variadic]) -> t
val index_of: t -> t -> ?start:int -> unit -> int
val last_index_of: t -> t -> ?start:int -> unit -> int
val length: t -> int
val locale_compare: t -> t -> int
val match_: t -> regexp -> t array option
val replace: t -> regexp -> t -> t
val search: t -> regexp -> int
val slice: t -> start:int -> ?end_:int -> unit -> t
val split: t -> ?separator:t -> ?limit:int -> unit -> t array
val substr: t -> start:int -> ?length:int -> unit -> t
val substring: t -> start:int -> ?end_:int -> unit -> t
val to_locale_lower_case: t -> t [@@js.call]
val to_locale_upper_case: t -> t [@@js.call]
val to_lower_case: t -> t [@@js.call]
val to_upper_case: t -> t [@@js.call]
val trim: t -> t [@@js.call]


(** {2 Regexps} *)

val regexp: t -> ?global:unit -> ?ignore_case:unit -> ?multiline:unit -> unit -> regexp
 [@@js.custom

      val regexp_internal: t -> ?flags:t -> unit -> regexp [@@js.new "RegExp"]

      let regexp txt ?global ?ignore_case ?multiline () =
        let l = [] in
        let l = match global with Some () -> of_string "g" :: l | None -> l in
        let l = match ignore_case with Some () -> of_string "i" :: l | None -> l in
        let l = match multiline with Some () -> of_string "m" :: l | None -> l in
        regexp_internal txt ~flags:(concat (of_string "") l) ()
 ]


val global: regexp -> bool
val ignore_case: regexp -> bool
val multiline: regexp -> bool
val source: regexp -> string
val last_index: regexp -> int
val exec: regexp -> t -> t array option
val test: regexp -> t -> bool
