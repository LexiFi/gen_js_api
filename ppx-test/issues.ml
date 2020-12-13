module Issue116 = [%js: type t]
module Issue117 = [%js:
  module T: sig
    val log: 'a -> unit [@@js.global]
    val log2: 'a -> 'b -> unit [@@js.global "jsLog2"]
  end [@js.scope "console"]
]
module Issue124 = [%js:
  type 'a dummy

  type u =
    | Unknown of Ojs.t [@js.default]
    | T of t [@js "t"]
    [@@js.union on_field "type"]

  and t = [`U of u] dummy [@@js.custom
    let t_to_js x = Obj.magic x
    let t_of_js x = Obj.magic x
  ]
]