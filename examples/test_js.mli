(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

(** Some ad hoc code to illustrate and test various aspects
    of gen_js_api *)

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
      [@@js.custom
          val get_context: t -> string -> Ojs.t
          [@@js.call]

          let getContext_2d x =
            get_context x "2d"
      ]
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
  [@@js.call "log"]
end

val console: Console.t

module Person: sig
  module Foo: sig
    type t =
      | Foo
      | Bar [@js 42]
      | OtherInt of int [@js.default]
      | OtherString of string [@js.default]
            [@@js.enum]
  end

  type t = private Ojs.t

  val create: string -> Foo.t -> t
  [@@js.new "Person"]

  val name: t -> string
  val foo: t -> Foo.t
  val get: t -> unit -> string * Foo.t
  [@@js.call]
  val set: t -> string * Foo.t -> unit
      [@@js.call]

  val cast: t -> Ojs.t [@@js.cast]
end

module PersonObj: sig
  class t: Ojs.t ->
     object
       inherit Ojs.obj
       method name: string
       method set_name: string -> unit
       method foo: Person.Foo.t
       method set_foo: Person.Foo.t -> unit
       method get: string * Person.Foo.t [@@js.call]
       method set: string * Person.Foo.t -> unit [@@js.call]
     end

  class person: string -> Person.Foo.t -> (int list [@js.variadic]) -> t

  val create: string -> Person.Foo.t -> t
  [@@js.new "Person"]

  val of_person: Person.t -> t
  [@@js.cast]
end

module Str: sig
  class t: Ojs.t ->
    object
      inherit Ojs.obj
      method concat: (t list [@js.variadic]) -> t
      method to_string: string [@@js.call]
    end

  class str: string -> t [@@js.new "String"]

  val create: string -> t
  [@@js.new "String"]
end

module Date: sig
  type t = private Ojs.t

  val create: year:int -> month:int -> ?day:(int[@js.default 0]) -> unit -> t [@@js.new "Date"]
  val to_string: t -> string [@@js.call]
end

module Person2: sig
  type t = private Ojs.t

  val mk: ?children:t list -> age:int -> (string[@js "name"]) -> t
  [@@js.builder]

  val to_json:  t -> string
  [@@js.global "JSON.stringify"]
end

type int_or_string_or_null =
  | Int of int
  | String of string
  | Nothing
    [@@js.union]

val f: ([`Int of int | `String of string | `Nothing] [@js.union]) -> unit

val g: int_or_string_or_null -> unit [@@js.global]

module Verb1: sig
  type t1 =
    { x_coord: int;
      y_coord: int;
    }

  class t2: Ojs.t ->
    object
      inherit Ojs.obj
      method x_coord: int
      method y_coord: int
    end
end [@js.verbatim_names]

module Verb2: sig
  type t1 =
    { x_coord: int;
      y_coord: int;
    } [@@js.verbatim_names]

  class t2: Ojs.t ->
    object
      inherit Ojs.obj
      method x_coord: int
      method y_coord: int
    end [@@js.verbatim_names]
end

module Console2: sig
  val log: string -> unit
    [@@js.global]
end [@js.scope "console"]

module Location: sig
  val hash: unit -> string
  val set_hash: string -> unit
end [@js.scope "location"]

module Location2: sig
  val hash: unit -> string [@@js.get]
  val set_hash: string -> unit [@@js.set]
end [@js.scope "location"]

module Location3: sig
  val assign: string -> unit
  val reload: ?force:bool -> unit -> unit
  val replace: string -> unit
end [@js.scope "location"]

module Union: sig
  type close_path

  type moveto_abs

  type svg_path_seg =
    | Unknown of Ojs.t         [@js.default]
    | Close_path of close_path [@js 1]
    | Moveto_abs of moveto_abs [@js 2]
          [@@js.union on_field "pathSegType"]
end
