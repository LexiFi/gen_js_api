(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

(** Partial binding to jQuery, serving as an illustration
    of gen_js_api.  The binding is far from complete! *)



(** {2 Sets of elements} *)

type t = private Ojs.t

val selector: string -> t
    [@@js.global "jQuery"]
(** Either select a set of elements from the current document, or
    create a new element (if given a string such as "<div>". *)


val text: t -> string
    [@@js.call]

val set_text: t -> string -> unit
    [@@js.call "text"]

val update_text: t -> (int -> string -> string) -> unit
    [@@js.call "text"]

val append_html: t -> string -> unit
    [@@js.call "append"]

val append: t -> (t list [@js.variadic]) -> unit
    [@@js.call "append"]

val get_val: t -> string
    [@@js.call "val"]

val hide: t -> unit
    [@@js.call "hide"]

val detach: t -> unit
    [@@js.call]

val remove: t -> unit
    [@@js.call]

val empty: t -> unit
    [@@js.call]


(** {2 Animations} *)

val fade_in: t -> ?duration:int -> ?finished:(unit -> unit) -> unit -> unit
    [@@js.call]
val fade_out: t -> ?duration:int -> ?finished:(unit -> unit) -> unit -> unit
    [@@js.call]

(** {2 Events} *)

module Event : sig
  type t = private Ojs.t

  val page_x: t -> float
  val page_y: t -> float
  val type_: t -> string
end

val on: t -> string -> (Event.t -> unit) -> unit

val ready: (unit -> unit) -> unit
   [@@js.global "jQuery"]

(** {2 AJAX} *)

module Ajax : sig
  type settings = private Ojs.t
    (** The type describing all settings of an AJAX call. *)

  type t = private Ojs.t
    (** Corresponds to jQuery's jqXHR object. *)

  val settings:
    ?complete:(t -> string -> unit) ->
    ?data:Ojs.t -> ?data_type:string ->
    ?meth:([`GET | `POST | `PUT] [@js "method"] [@js.enum]) ->
    ?url:string ->
    unit -> settings
    [@@js.builder]

  val run: settings -> unit
    [@@js.global "jQuery.ajax"]

  val response_text: t -> string
    [@@js.get]
end
