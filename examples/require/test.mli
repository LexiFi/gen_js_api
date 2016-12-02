
module A : sig
  val show_int : int -> unit
  val prop : unit -> string
  val set_prop : string -> unit
  val add : int -> int -> int
end [@@js.require "./a"]

module B : sig
  class b : Ojs.t ->
    object
      inherit Ojs.obj
      method incr : unit -> int
      method decr : unit -> int
    end

  val new_b : unit -> b
end [@@js.require "./b"]

module C : sig
  type t = private Ojs.t
  val incr : t -> int -> int
  val show_string : t -> string -> unit
  val prop : t -> int
  val set_prop : t -> int -> unit
end

val module_c : C.t Lazy.t [@@js.require "./c"]
