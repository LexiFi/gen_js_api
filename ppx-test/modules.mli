module Event: sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
end

module Foo: sig
  module E = Event

  val foo: E.t -> string -> unit [@@js.call]
end

module Bar: sig
  include (module type of Event)

  val bar: t -> string -> unit [@@js.call]
end
