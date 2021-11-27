
type 'a of_js = Ojs.t -> 'a
type 'a to_js = 'a -> Ojs.t

(** JS-able types *)

let _ : string of_js = [%js.to: string]
let _ : string to_js = [%js.of: string]

let _ : int of_js = [%js.to: int]
let _ : int to_js = [%js.of: int]
let _ : bool of_js = [%js.to: bool]
let _ : bool to_js = [%js.of: bool]
let _ : float of_js = [%js.to: float]
let _ : float to_js = [%js.of: float]
let _ : Ojs.t of_js = [%js.to: Ojs.t]
let _ : Ojs.t to_js = [%js.of: Ojs.t]
let _ : (string * int) of_js = [%js.to: string * int]
let _ : (string * int) to_js = [%js.of: string * int]
let _ : (string * int * bool) of_js = [%js.to: string * int * bool]
let _ : (string * int * bool) to_js = [%js.of: string * int * bool]
let _ : (string -> int) of_js = [%js.to: string -> int]
let _ : (string -> int) to_js = [%js.of: string -> int]
let _ : ((string -> int) -> bool -> unit) of_js = [%js.to: (string -> int) -> bool -> unit]
let _ : ((string -> int) -> bool -> unit) to_js = [%js.of: (string -> int) -> bool -> unit]
let _ : (string array) of_js = [%js.to: string array]
let _ : (string array) to_js = [%js.of: string array]
let _ : (string list) of_js = [%js.to: string list]
let _ : (string list) to_js = [%js.of: string list]
let _ : (string option) of_js = [%js.to: string option]
let _ : (string option) to_js = [%js.of: string option]
let _ : (_ -> _) of_js = [%js.to: 'a -> 'b]
let _ : (_ -> _) to_js = [%js.of: 'a -> 'b]
let _ : [`foo | `bar | `Baz | `I of int | `S of string ] of_js = [%js.to: [`foo | `bar [@js 42] | `Baz | `I of int [@js.default] | `S of string[@js.default] ] [@js.enum]]
let _ : [`foo | `bar | `Baz | `I of int | `S of string ] to_js = [%js.of: [`foo | `bar [@js 42] | `Baz | `I of int [@js.default] | `S of string[@js.default] ] [@js.enum]]

(** Label & Options Value *)

let _ : (label:int -> ?opt:int -> unit -> unit) of_js = [%js.to: label:int -> ?opt:int -> unit -> unit]
let _ : (label:int -> ?opt:int -> unit -> unit) to_js = [%js.of: label:int -> ?opt:int -> unit -> unit]
let _ : (label:int -> ?opt:int -> unit -> unit) of_js =  [%js.to: label:int -> ?opt:int -> unit -> unit] (* js.default is ignored *)
let _ : (label:int -> ?opt:int -> unit -> unit) to_js = [%js.of: label:int -> ?opt:int -> unit -> unit]  (* js.default is ignored *)

(** Functions *)

module B  = [%js:
    val default0: ?x:int -> unit -> unit [@@js.global]
    val default1: ?x:(int[@js.default 42]) -> unit -> unit [@@js.global]

    val builder0: unit -> Ojs.t [@@js.builder]
    val builder1: x:int -> Ojs.t [@@js.builder]
    val builder2: ?x:int -> ?y:string -> unit -> Ojs.t [@@js.builder]
    val builder3: x:int -> y:string -> unit -> Ojs.t [@@js.builder]
    val builder4: x:int -> y:string -> z:unit -> Ojs.t [@@js.builder]
    val builder5: ?x:int -> ?y:string -> unit -> Ojs.t [@@js.builder]
    val builder6: ?x:(int [@js.default 42]) -> ?y:(string [@js.default "42"]) -> ?z:int -> unit -> Ojs.t [@@js.builder]

    val sep: string -> (string list [@js.variadic]) -> string [@@js.global]
]


(** Types Declarations *)
module T  = [%js:
    type js = private Ojs.t

    type abstract

    type alias = js

    type private_alias = private alias

    type record = { x: js; y: js }

    type mutable_record = { mutable x: js; y: js }

    type record_relabel =  { x : int; y : int [@js "Y"]}

    type ('a, 'b) parametrized = { x : 'a; y : 'b }

    type 'a abs = ('a -> int) -> unit

    type specialized = (int, int) parametrized

    type enum =
      | Foo [@js "foo"]
      | Bar [@js 42]
      | Baz [@js 4.2]
      | Qux
        [@@js.enum]

    type status =
    | OK [@js 1]
    | KO [@js 2]
    | OO [@js 1.5]
    | OtherS of string [@js.default]
    | OtherI of int [@js.default]
      [@@js.enum]

    type poly = [`foo | `bar [@js 42] | `baz [@js 4.2] | `Qux | `I of int [@js.default] | `S of string[@js.default]] [@js.enum]

    type sum =
        | A
        | B of int
        | C of int * string
        | D of {age: int; name: string}
        | Unknown of Ojs.t [@js.default]
    [@@js.sum]

    type t =
      | A [@js "A"]
      | B of int [@js.arg "bArg"]
      | C of int * string [@js.arg "cArg"]
      | D of {age: int [@js "X"]; name: string [@js "Y"]}
      | E of int [@js "F"][@js.arg "fArg"]
      | Unknown of Ojs.t [@js.default]
    [@@js.sum "kind"]

    type union =
      A | B of int | C of int | D of Ojs.t [@js.default] [@@js.union]

    type poly_union =
      [`A | `B of int | `C of int | `D of Ojs.t [@js.default]] [@js.union]

    type discr_union =
      A | B of int | C of int | D of Ojs.t [@js.default] [@@js.union on_field "discr"]

    type discr_poly_union =
      [`A | `B of int | `C of int | `D of Ojs.t [@js.default]] [@js.union on_field "discr"]

    type discr_union_value =
      A [@js 0] | B of int [@js "42"] | C of int | D of Ojs.t [@js.default] [@@js.union on_field "discr"]

]
