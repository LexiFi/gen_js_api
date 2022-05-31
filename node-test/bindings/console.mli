[@@@js.scope "console"]

val log: 'a -> unit [@@js.global]
val error: 'a -> unit [@@js.global]

module T : sig
  val log: (module[@js] Ojs.T with type t = 'a) -> 'a -> unit [@@js.global]
  val error: (module[@js] Ojs.T with type t = 'a) -> 'a -> unit [@@js.global]
end