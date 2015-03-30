(* Default conventions:

 - val xxx : t -> T
   Getter for property "xxx" on object of type "t"

 - val set_xxx : t -> T -> unit
   Setter for property "xxx" on object of type "t"

 - val xxx : t -> unit -> T
   Call method "xxx" on object of type "t" with no arguments

 - val xxx : t -> T1 -> ... -> Tn -> T
   Call method "xxx" on object of type "t" with n arguments
*)


module Element : sig
  type t = private Ojs.t

  val appendChild: t -> t -> unit
  val set_innerHTML: t -> string -> unit
  val innerHTML: t -> string

  val set_onclick: t -> (unit -> unit) -> unit
  val setAttribute: t -> string -> string -> unit
end

module Canvas : sig
  module RenderingContext2D : sig
    type t = private Ojs.t

    val set_fillStyle: t -> string -> unit
    val fillRect: t -> int -> int -> int -> int -> unit
  end

  type t = private Ojs.t

  val of_element: Element.t -> t
      [@@js.cast]

  val getContext_2d: t -> RenderingContext2D.t
      [@@js.expr call arg0 "getContext" "2d"]
end


module Document : sig
  type t = private Ojs.t

  val set_title: t -> string -> unit
  val title: t -> string

  val getElementById: t -> string -> Element.t
  val getElementsByClassName: t -> string -> Element.t array

  val createElement: t -> string -> Element.t
  val createTextNode: t -> string -> Element.t

  val body: t -> Element.t
end

module Window : sig
  type t = private Ojs.t

  val __: t
    [@@js.expr global "window"]

  val document: t -> Document.t
end

val alert: string -> unit
val setTimeout: (unit -> unit) -> int -> unit
