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

  type ('a, 'b) base = [ `BaseA of 'a | `BaseB of 'b ] dummy [@@js.custom {
    to_js = (fun _ _ -> Obj.magic);
    of_js = (fun _ _ -> Obj.magic)
  }]
  and base1 = (int, string) base
  and base2 = (string, int) base
]
module Issue109 = [%js:
  type t =
    [ `S of string [@js.default]
    | `I of int [@js.default]
    ] [@@js.enum]
]
module Issue142 = [%js:
  type t = [`Foo [@js 42]] [@js.enum]
  and u = t
]
module Issue144 = [%js:
  type t
  val f: t -> (args:int -> int [@js.dummy]) [@@js.call "f"]
]
module Issue146 = [%js:
  val f: ?arg:([`Foo [@js 42]] [@js.enum]) -> unit -> int [@@js.global "f"]
]
module PR165 = [%js:
  module Markdown : sig
    type t
  end

  module [@js.scope "ParameterInformation"] ParameterInformation : sig
    type t
    val create: label:([`String of string | `Tuple of (int * int)] [@js.union]) -> ?documentation:([`String of string | `Markdown of Markdown.t] [@js.union]) -> unit -> t [@@js.create]
  end
]