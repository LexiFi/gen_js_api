module JsArray (E: Ojs.T): sig
  type t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t

  val create: unit -> t [@@js.new "Array"]
  val push: t -> E.t -> unit [@@js.call]
  val pop: t -> E.t option [@@js.call]
end

module UntypedArray : sig
  include (module type of JsArray(Ojs))
end

module StringArray : sig
  include (module type of JsArray(Ojs.String))

  val join: t -> string -> string [@@js.call]
end

module[@js.scope "Array"] JsArray2: sig
  type 'a t
  val t_to_js: ('a -> Ojs.t) -> 'a t -> Ojs.t
  val t_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a t

  val create: unit -> 'a t [@@js.create]
  val create': (module[@js] Ojs.T with type t = 'a) -> ('a list [@js.variadic]) -> 'a t [@@js.create]

  val push: (module[@js] Ojs.T with type t = 'a) -> 'a t -> 'a -> unit [@@js.call]
  val pop:  (module[@js] Ojs.T with type t = 'a) -> 'a t -> 'a option [@@js.call]

  val get: (module[@js] Ojs.T with type t = 'a) -> 'a t -> int -> 'a option [@@js.index_get]
  val set: (module[@js] Ojs.T with type t = 'a) -> 'a t -> int -> 'a -> unit [@@js.index_set]

  val join: string t -> string -> string [@@js.call]
end