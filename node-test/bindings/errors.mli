module [@js.scope] Error : sig
  type t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t

  [@@@js.static]
  val create: string -> t
  val stack_trace_limit: int
  val set_stack_trace_limit: int -> unit

  [@@@js.prototype]
  val code: t -> string
  val message: t -> string
  val stack: t -> string
end
