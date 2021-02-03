module Issue116 = [%js: type t]
module Issue117 = [%js:
  module T: sig
    val log: 'a -> unit [@@js.global]
    val log2: 'a -> 'b -> unit [@@js.global "jsLog2"]
  end [@js.scope "console"]
]
module Issue124 = [%js:
  type a
  and b =
    { a : a }
    [@@js.custom {
      to_js = (fun { a } -> [%js.of: a] a);
      of_js = (fun js -> { a = [%js.to: a] js})
    }]

  type 'a dummy

  type 'a wrapped =
    | Wrapped of 'a
    [@@js.custom {
      to_js = (
        let f a_to_js =
          function Wrapped a ->
            a_to_js a
        in f);
      of_js = (
        let f a_of_js x =
          Wrapped (a_of_js x)
        in f
      )
    }]

  type u =
    | Unknown of Ojs.t [@js.default]
    | T of t [@js "t"]
    | WrappedT of t wrapped [@js "wrapped_t"]
    [@@js.union on_field "type"]

  and t = [`U of u] dummy [@@js.custom {
    to_js = Obj.magic; of_js = Obj.magic
  }]


]