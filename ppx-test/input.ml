include [%js:
module T: sig
  val log: 'a -> unit [@@js.global]
  val log2: 'a -> 'b -> unit [@@js.global "jsLog2"]
end [@js.scope "console"]

module M: sig
  type t
end
]
