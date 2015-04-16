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

  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val document: t -> Document.t

  val set_onload: t -> (unit -> unit) -> unit
end

val window: Window.t

val alert: string -> unit
  [@@js.global]

val setTimeout: (unit -> unit) -> int -> unit

module Console: sig
  type t = private Ojs.t

  val log: t -> Ojs.t -> unit

  val log_string: t -> string -> unit
  [@@js.meth "log"]
end

val console: Console.t

module Person: sig
  module Foo: sig
    type t =
      | Foo
      | Bar [@js 42]
      | OtherInt of int [@js.default]
      | OtherString of string [@js.default]
  end

  type t = private Ojs.t

  val create: string -> Foo.t -> t
  [@@js.new "Person"]

  val name: t -> string
  val foo: t -> Foo.t
  val get: t -> unit -> string * Foo.t
  [@@js.meth]
  val set: t -> string * Foo.t -> unit
  [@@js.meth]
end

module PersonObj: sig
  class t: Ojs.t ->
     object
       inherit Ojs.obj
       method name: string
       method set_name: string -> unit
       method foo: Person.Foo.t
       method set_foo: Person.Foo.t -> unit
       method get: string * Person.Foo.t [@@js.meth]
       method set: string * Person.Foo.t -> unit [@@js.meth]
     end

  val create: string -> Person.Foo.t -> t
  [@@js.new "Person"]

  val of_person: Person.t -> t
  [@@js.cast]
end

module Str: sig
  class t: Ojs.t ->
    object
      inherit Ojs.obj
      method char_at: int -> t
      method char_code_at: int -> int
      method concat: (t list [@js.variadic]) -> t
      method from_char_code: (int list [@js.variadic]) -> t
      method index_of: t -> int
      method index_of_from: t -> int -> int [@@js.meth "indexOf"]
      method last_index_of: t -> int
      method last_index_of_from: t -> int -> int [@@js.meth "indexOf"]
      method length: int
      method locale_compare: t -> int
      method slice: int -> int -> t
      method split: t -> t array
      method substr: int -> int -> t
      method substring: int -> int -> t
      method to_locale_lower_case: t [@@js.meth]
      method to_locale_upper_case: t [@@js.meth]
      method to_lower_case: t [@@js.meth]
      method to_upper_case: t [@@js.meth]
      method to_string: string [@@js.meth]
      method value_of: string [@@js.meth]
    end

  val create: string -> t
  [@@js.new "String"]
end
