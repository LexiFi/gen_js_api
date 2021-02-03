module [@js.scope] Error : sig
  type t [@@js]

  val create: string -> t [@@js.create]
  val stack_trace_limit: int [@@js.global]
  val set_stack_trace_limit: int -> unit [@@js.set]

  val code: t -> string [@@js.get]
  val message: t -> string [@@js.get]
  val stack: t -> string [@@js.get]
end
