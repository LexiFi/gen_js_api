module[@js.scope "console"] Console: sig
  val log: (module[@js] Ojs.T with type t = 'a) -> 'a -> unit [@@js.global "log"]
  val log2:
    (module[@js] Ojs.T with type t = 'a) ->
    (module[@js] Ojs.T with type t = 'b) ->
    'a -> 'b -> unit [@@js.global "log"]
  val log3:
    (module[@js] Ojs.T with type t = 'a) ->
    (module[@js] Ojs.T with type t = 'b) ->
    (module[@js] Ojs.T with type t = 'c) ->
    'a -> 'b -> 'c -> unit [@@js.global "log"]
end

module Console2: sig
  type t
  val log: (module[@js] Ojs.T with type t = 'a) -> t -> 'a -> unit [@@js.call "log"]
  val log2:
    (module[@js] Ojs.T with type t = 'a) ->
    (module[@js] Ojs.T with type t = 'b) ->
    t -> 'a -> 'b -> unit [@@js.call "log"]
  val log3:
    (module[@js] Ojs.T with type t = 'a) ->
    (module[@js] Ojs.T with type t = 'b) ->
    (module[@js] Ojs.T with type t = 'c) ->
    t -> 'a -> 'b -> 'c -> unit [@@js.call "log"]
end

module[@js.scope "console"] Console3: sig
  module [@js.scope "log"] Log: sig
    val _1: (module[@js] Ojs.T with type t = 'a) -> 'a -> unit [@@js.invoke]
    val _2:
      (module[@js] Ojs.T with type t = 'a) ->
      (module[@js] Ojs.T with type t = 'b) ->
      'a -> 'b -> unit [@@js.invoke]
    val _3:
      (module[@js] Ojs.T with type t = 'a) ->
      (module[@js] Ojs.T with type t = 'b) ->
      (module[@js] Ojs.T with type t = 'c) ->
      'a -> 'b -> 'c -> unit [@@js.invoke]
  end
end

module[@js.scope "Array"] Array: sig
  type 'a t
  val t_to_js: ('a -> Ojs.t) -> 'a t -> Ojs.t
  val t_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a t

  val create:  (module[@js] Ojs.T with type t = 'a) -> ('a list [@js.variadic]) -> 'a t [@@js.create]
  val create': (module[@js] Ojs.T with type t = 'a) -> ('a list [@js.variadic]) -> 'a t [@@js.invoke]

  val push: (module[@js] Ojs.T with type t = 'a) -> 'a t -> 'a -> unit [@@js.call]
  val pop:  (module[@js] Ojs.T with type t = 'a) -> 'a t -> 'a option [@@js.call]
end