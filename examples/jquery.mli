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
