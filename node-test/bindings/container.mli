module StringMap : sig
  include Map.S with type key = string
  val t_to_js: ('a -> Ojs.t) -> 'a t -> Ojs.t
  val t_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a t
end