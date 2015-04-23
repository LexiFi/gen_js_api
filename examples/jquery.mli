(** {2 Sets of elements} *)

type t = private Ojs.t

val selector: string -> t
    [@@js.global "jQuery"]
(** Either select a set of elements from the current document, or
    create a new element (if given a string such as "<div>". *)


val text: t -> string
    [@@js.meth]

val set_text: t -> string -> unit
    [@@js.meth "text"]


val update_text: t -> (int -> string -> string) -> unit
    [@@js.meth "text"]


val append_html: t -> string -> unit
    [@@js.meth "append"]

val append: t -> t -> unit
    [@@js.meth "append"]


val get_val: t -> string
  [@@js.meth "val"]


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
    (** Correponds to jQuery's jqXHR object. *)

  val settings: unit -> settings
    [@@js.new "Object"]

  val complete: settings -> (t -> string -> unit) -> unit
    [@@js.set "complete"]

  val data: settings -> Ojs.t -> unit
    [@@js.set "data"]

  val data_type: settings -> string -> unit
    [@@js.set "dataType"]

  val meth: settings -> [`GET|`POST|`PUT] -> unit
    [@@js.set "method"]

  val url: settings -> string -> unit
    [@@js.set "url"]


(* with an object builder, this could be:
  val settings: ?complete:(t -> string -> unit) -> ?data:Ojs.t -> ... -> ?url:string -> settings
*)


  val run: settings -> unit
    [@@js.global "jQuery.ajax"]

  val response_text: t -> string
    [@@js.get]
end
