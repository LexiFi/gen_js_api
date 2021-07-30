module [@js.scope "Foo"] rec Foo : sig
  type t = private Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t
  val create: string -> t [@@js.create]
  val describe: t -> string [@@js.call "describe"]
  val to_bar: t -> Bar.t [@@js.call "toBar"]
end

and [@js.scope "Bar"] Bar : sig
  type t = private Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t
  val create: string -> t [@@js.create]
  val describe: t -> string [@@js.call "describe"]
  val to_foo: t -> Foo.t [@@js.call "toFoo"]
end
