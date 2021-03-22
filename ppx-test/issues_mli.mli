module Issue144: sig
  type t
  val f: t -> (args:int -> int [@js.dummy]) [@@js.call "f"]
end
