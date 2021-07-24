type t = private Ojs.t

val toString: t -> ?radix:int -> unit -> float [@@js.call]
val toFixed: t -> ?fractionDigits:int -> unit -> float [@@js.call]
val toExponential: t -> ?fractionDigits:int -> unit -> float [@@js.call]
val toPrecision: t -> ?precision:int -> unit -> float [@@js.call]
val valueOf: t -> float [@@js.call]

(* scoped *)

module [@js.scope "Number"] Scoped : sig
  val create: 'any -> t [@@js.create]
  val invoke: 'any -> float [@@js.invoke]

  val min_value: float [@@js.global "MIN_VALUE"]
  val max_value: float [@@js.global "MAX_VALUE"]
  val nan: float [@@js.global "NaN"]
  val negative_infinity: float [@@js.global "NEGATIVE_INFINITY"]
  val positive_infinity: float [@@js.global "POSITIVE_INFINITY"]
end

(* non-scoped *)

module Static : sig
  type number = t
  type t = private Ojs.t

  val create: t -> 'any -> number [@@js.apply_newable]
  val apply: t -> 'any -> float [@@js.apply]

  val min_value: t -> float [@@js.get "MIN_VALUE"]
  val max_value: t -> float [@@js.get "MAX_VALUE"]
  val nan: t -> float [@@js.get "NaN"]
  val negative_infinity: t -> float [@@js.get "NEGATIVE_INFINITY"]
  val positive_infinity: t -> float [@@js.get "POSITIVE_INFINITY"]
end
val number: Static.t [@@js.global "Number"]