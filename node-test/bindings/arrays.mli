module JsArray (E:
  sig
    type t
    val t_to_js: t -> Ojs.t
    val t_of_js: Ojs.t -> t
  end): sig
  type t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t

  val create: unit -> t [@@js.new "Array"]
  val push: t -> E.t -> unit [@@js.call]
  val pop: t -> E.t option [@@js.call]
end

module JsString : sig
  type t = string
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
end

module UntypedArray : sig
  include (module type of JsArray(Ojs))
end

module StringArray : sig
  include (module type of JsArray(JsString))

  val join: t -> string -> string [@@js.call]
end