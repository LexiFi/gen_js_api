module Issue116 = struct 
  type t
  type s = int [@@js]
  module M = [%js: type t type s[@@js]]
end
module Issue117 = [%js:
  module T: sig
    val log: 'a -> unit [@@js.global]
    val log2: 'a -> 'b -> unit [@@js.global "jsLog2"]
  end [@js.scope "console"]
]
