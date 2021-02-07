type 'a of_js = Ojs.t -> 'a
type 'a to_js = 'a -> Ojs.t
[@@@ocaml.text " JS-able types "]
let (_ : string of_js) = Ojs.string_of_js
let (_ : string to_js) = Ojs.string_to_js
let (_ : int of_js) = Ojs.int_of_js
let (_ : int to_js) = Ojs.int_to_js
let (_ : bool of_js) = Ojs.bool_of_js
let (_ : bool to_js) = Ojs.bool_to_js
let (_ : float of_js) = Ojs.float_of_js
let (_ : float to_js) = Ojs.float_to_js
let (_ : Ojs.t of_js) = fun x9 -> x9
let (_ : Ojs.t to_js) = fun x10 -> x10
let (_ : (string * int) of_js) =
  fun x11 ->
    let x12 = x11 in
    ((Ojs.string_of_js (Ojs.array_get x12 0)),
      (Ojs.int_of_js (Ojs.array_get x12 1)))
let (_ : (string * int) to_js) =
  fun x13 ->
    let (x14, x15) = x13 in
    let x16 = Ojs.array_make 2 in
    Ojs.array_set x16 0 (Ojs.string_to_js x14);
    Ojs.array_set x16 1 (Ojs.int_to_js x15);
    x16
let (_ : (string * int * bool) of_js) =
  fun x17 ->
    let x18 = x17 in
    ((Ojs.string_of_js (Ojs.array_get x18 0)),
      (Ojs.int_of_js (Ojs.array_get x18 1)),
      (Ojs.bool_of_js (Ojs.array_get x18 2)))
let (_ : (string * int * bool) to_js) =
  fun x19 ->
    let (x20, x21, x22) = x19 in
    let x23 = Ojs.array_make 3 in
    Ojs.array_set x23 0 (Ojs.string_to_js x20);
    Ojs.array_set x23 1 (Ojs.int_to_js x21);
    Ojs.array_set x23 2 (Ojs.bool_to_js x22);
    x23
let (_ : (string -> int) of_js) =
  fun x24 ->
    fun x25 -> Ojs.int_of_js (Ojs.apply x24 [|(Ojs.string_to_js x25)|])
let (_ : (string -> int) to_js) =
  fun x26 ->
    Ojs.fun_to_js 1 (fun x27 -> Ojs.int_to_js (x26 (Ojs.string_of_js x27)))
let (_ : ((string -> int) -> bool -> unit) of_js) =
  fun x28 ->
    fun x29 ->
      fun x31 ->
        ignore
          (Ojs.apply x28
             [|(Ojs.fun_to_js 1
                  (fun x30 -> Ojs.int_to_js (x29 (Ojs.string_of_js x30))));(
               Ojs.bool_to_js x31)|])
let (_ : ((string -> int) -> bool -> unit) to_js) =
  fun x32 ->
    Ojs.fun_to_js 2
      (fun x33 ->
         fun x35 ->
           x32
             (fun x34 ->
                Ojs.int_of_js (Ojs.apply x33 [|(Ojs.string_to_js x34)|]))
             (Ojs.bool_of_js x35))
let (_ : string array of_js) =
  fun x36 -> Ojs.array_of_js Ojs.string_of_js x36
let (_ : string array to_js) =
  fun x38 -> Ojs.array_to_js Ojs.string_to_js x38
let (_ : string list of_js) = fun x40 -> Ojs.list_of_js Ojs.string_of_js x40
let (_ : string list to_js) = fun x42 -> Ojs.list_to_js Ojs.string_to_js x42
let (_ : string option of_js) =
  fun x44 -> Ojs.option_of_js Ojs.string_of_js x44
let (_ : string option to_js) =
  fun x46 -> Ojs.option_to_js Ojs.string_to_js x46
let (_ : (_ -> _) of_js) =
  fun x48 -> fun x49 -> Obj.magic (Ojs.apply x48 [|(Obj.magic x49)|])
let (_ : (_ -> _) to_js) =
  fun x50 -> Ojs.fun_to_js 1 (fun x51 -> Obj.magic (x50 (Obj.magic x51)))
let (_ : [ `foo  | `bar  | `Baz  | `I of int  | `S of string ] of_js) =
  fun x52 ->
    let x53 = x52 in
    match Ojs.type_of x53 with
    | "number" -> (match Ojs.int_of_js x53 with | 42 -> `bar | x54 -> `I x54)
    | "string" ->
        (match Ojs.string_of_js x53 with
         | "foo" -> `foo
         | "Baz" -> `Baz
         | x55 -> `S x55)
    | _ -> assert false
let (_ : [ `foo  | `bar  | `Baz  | `I of int  | `S of string ] to_js) =
  fun x56 ->
    match x56 with
    | `foo -> Ojs.string_to_js "foo"
    | `bar -> Ojs.int_to_js 42
    | `Baz -> Ojs.string_to_js "Baz"
    | `I x57 -> Ojs.int_to_js x57
    | `S x58 -> Ojs.string_to_js x58
[@@@ocaml.text " Label & Options Value "]
let (_ : (label:int -> ?opt:int -> unit -> unit) of_js) =
  fun x59 ->
    fun ~label:x60 ->
      fun ?opt:x61 ->
        fun () ->
          ignore
            (Ojs.call x59 "apply"
               [|Ojs.null;((let x62 =
                              Ojs.new_obj
                                (Ojs.get_prop_ascii Ojs.global "Array") 
                                [||] in
                            ignore
                              (Ojs.call x62 "push" [|(Ojs.int_to_js x60)|]);
                            (match x61 with
                             | Some x63 ->
                                 ignore
                                   (Ojs.call x62 "push"
                                      [|(Ojs.int_to_js x63)|])
                             | None -> ());
                            x62))|])
let (_ : (label:int -> ?opt:int -> unit -> unit) to_js) =
  fun x64 ->
    Ojs.fun_to_js 2
      (fun x65 ->
         fun x66 ->
           x64 ~label:(Ojs.int_of_js x65)
             ?opt:(Ojs.option_of_js Ojs.int_of_js x66) ())
let (_ : (label:int -> ?opt:int -> unit -> unit) of_js) =
  fun x68 ->
    fun ~label:x69 ->
      fun ?opt:x70 ->
        fun () ->
          ignore
            (Ojs.call x68 "apply"
               [|Ojs.null;((let x71 =
                              Ojs.new_obj
                                (Ojs.get_prop_ascii Ojs.global "Array") 
                                [||] in
                            ignore
                              (Ojs.call x71 "push" [|(Ojs.int_to_js x69)|]);
                            (match x70 with
                             | Some x72 ->
                                 ignore
                                   (Ojs.call x71 "push"
                                      [|(Ojs.int_to_js x72)|])
                             | None -> ());
                            x71))|])
let (_ : (label:int -> ?opt:int -> unit -> unit) to_js) =
  fun x73 ->
    Ojs.fun_to_js 2
      (fun x74 ->
         fun x75 ->
           x73 ~label:(Ojs.int_of_js x74)
             ?opt:(Ojs.option_of_js Ojs.int_of_js x75) ())
[@@@ocaml.text " Functions "]
module B :
  sig
    val default0 : ?x:int -> unit -> unit
    val default1 : ?x:int -> unit -> unit
    val builder0 : unit -> Ojs.t
    val builder1 : x:int -> Ojs.t
    val builder2 : ?x:int -> ?y:string -> unit -> Ojs.t
    val builder3 : x:int -> y:string -> unit -> Ojs.t
    val builder4 : x:int -> y:string -> z:unit -> Ojs.t
    val builder5 : ?x:int -> ?y:string -> unit -> Ojs.t
    val builder6 : ?x:int -> ?y:string -> ?z:int -> unit -> Ojs.t
    val sep : string -> string list -> string
  end =
  ((struct
      [@@@js.dummy "!! This code has been generated by gen_js_api !!"]
      [@@@ocaml.warning "-7-32-39"]
      let (default0 : ?x:int -> unit -> unit) =
        fun ?x:x77 ->
          fun () ->
            ignore
              (let x80 = Ojs.global in
               Ojs.call (Ojs.get_prop_ascii x80 "default0") "apply"
                 [|x80;((let x78 =
                           Ojs.new_obj
                             (Ojs.get_prop_ascii Ojs.global "Array") 
                             [||] in
                         (match x77 with
                          | Some x79 ->
                              ignore
                                (Ojs.call x78 "push" [|(Ojs.int_to_js x79)|])
                          | None -> ());
                         x78))|])
      let (default1 : ?x:int -> unit -> unit) =
        fun ?x:x81 ->
          fun () ->
            ignore
              (Ojs.call Ojs.global "default1"
                 [|(Ojs.int_to_js
                      (match x81 with | Some x82 -> x82 | None -> 42))|])
      let (builder0 : unit -> Ojs.t) =
        fun () -> let x83 = Ojs.empty_obj () in x83
      let (builder1 : x:int -> Ojs.t) =
        fun ~x:x84 ->
          let x85 = Ojs.empty_obj () in
          Ojs.set_prop_ascii x85 "x" (Ojs.int_to_js x84); x85
      let (builder2 : ?x:int -> ?y:string -> unit -> Ojs.t) =
        fun ?x:x86 ->
          fun ?y:x87 ->
            fun () ->
              let x88 = Ojs.empty_obj () in
              (match x86 with
               | Some x90 -> Ojs.set_prop_ascii x88 "x" (Ojs.int_to_js x90)
               | None -> ());
              (match x87 with
               | Some x89 ->
                   Ojs.set_prop_ascii x88 "y" (Ojs.string_to_js x89)
               | None -> ());
              x88
      let (builder3 : x:int -> y:string -> unit -> Ojs.t) =
        fun ~x:x91 ->
          fun ~y:x92 ->
            fun () ->
              let x93 = Ojs.empty_obj () in
              Ojs.set_prop_ascii x93 "x" (Ojs.int_to_js x91);
              Ojs.set_prop_ascii x93 "y" (Ojs.string_to_js x92);
              x93
      let (builder4 : x:int -> y:string -> z:unit -> Ojs.t) =
        fun ~x:x94 ->
          fun ~y:x95 ->
            fun ~z:x96 ->
              let x97 = Ojs.empty_obj () in
              Ojs.set_prop_ascii x97 "x" (Ojs.int_to_js x94);
              Ojs.set_prop_ascii x97 "y" (Ojs.string_to_js x95);
              Ojs.set_prop_ascii x97 "z" (Ojs.unit_to_js x96);
              x97
      let (builder5 : ?x:int -> ?y:string -> unit -> Ojs.t) =
        fun ?x:x98 ->
          fun ?y:x99 ->
            fun () ->
              let x100 = Ojs.empty_obj () in
              (match x98 with
               | Some x102 ->
                   Ojs.set_prop_ascii x100 "x" (Ojs.int_to_js x102)
               | None -> ());
              (match x99 with
               | Some x101 ->
                   Ojs.set_prop_ascii x100 "y" (Ojs.string_to_js x101)
               | None -> ());
              x100
      let (builder6 : ?x:int -> ?y:string -> ?z:int -> unit -> Ojs.t) =
        fun ?x:x103 ->
          fun ?y:x104 ->
            fun ?z:x105 ->
              fun () ->
                let x106 = Ojs.empty_obj () in
                Ojs.set_prop_ascii x106 "x"
                  (Ojs.int_to_js
                     (match x103 with | Some x109 -> x109 | None -> 42));
                Ojs.set_prop_ascii x106 "y"
                  (Ojs.string_to_js
                     (match x104 with | Some x108 -> x108 | None -> "42"));
                (match x105 with
                 | Some x107 ->
                     Ojs.set_prop_ascii x106 "z" (Ojs.int_to_js x107)
                 | None -> ());
                x106
      let (sep : string -> string list -> string) =
        fun x113 ->
          fun x110 ->
            Ojs.string_of_js
              (let x114 = Ojs.string_to_js x113 in
               Ojs.call (Ojs.get_prop_ascii x114 "sep") "apply"
                 [|x114;((let x111 =
                            Ojs.new_obj
                              (Ojs.get_prop_ascii Ojs.global "Array") 
                              [||] in
                          List.iter
                            (fun x112 ->
                               ignore
                                 (Ojs.call x111 "push"
                                    [|(Ojs.string_to_js x112)|])) x110;
                          x111))|])
    end)[@merlin.hide ]) 
module T :
  sig
    type js = private Ojs.t
    type abstract
    type alias = js
    type private_alias = private alias
    type record = {
      x: js ;
      y: js }
    type mutable_record = {
      mutable x: js ;
      y: js }
    type record_relabel = {
      x: int ;
      y: int }
    type ('a, 'b) parametrized = {
      x: 'a ;
      y: 'b }
    type specialized = (int, int) parametrized
    type enum =
      | Foo 
      | Bar 
      | Baz 
    type status =
      | OK 
      | KO 
      | OtherS of string 
      | OtherI of int 
    type poly = [ `foo  | `bar  | `Baz  | `I of int  | `S of string ]
    type sum =
      | A 
      | B of int 
      | C of int * string 
      | D of {
      age: int ;
      name: string } 
      | Unknown of Ojs.t 
    type t =
      | A 
      | B of int 
      | C of int * string 
      | D of {
      age: int ;
      name: string } 
      | E of int 
      | Unknown of Ojs.t 
    type union =
      | A 
      | B of int 
      | C of int 
      | D of Ojs.t 
    type poly_union = [ `A  | `B of int  | `C of int  | `D of Ojs.t ]
    type discr_union =
      | A 
      | B of int 
      | C of int 
      | D of Ojs.t 
    type discr_poly_union = [ `A  | `B of int  | `C of int  | `D of Ojs.t ]
    type discr_union_value =
      | A 
      | B of int 
      | C of int 
      | D of Ojs.t 
  end =
  ((struct
      [@@@js.dummy "!! This code has been generated by gen_js_api !!"]
      [@@@ocaml.warning "-7-32-39"]
      type js = Ojs.t
      let rec (js_of_js : Ojs.t -> js) = fun x116 -> x116
      and (js_to_js : js -> Ojs.t) = fun x115 -> x115
      type abstract = Ojs.t
      let rec (abstract_of_js : Ojs.t -> abstract) = fun x118 -> x118
      and (abstract_to_js : abstract -> Ojs.t) = fun x117 -> x117
      type alias = js
      let rec (alias_of_js : Ojs.t -> alias) = js_of_js
      and (alias_to_js : alias -> Ojs.t) = js_to_js
      type private_alias = alias
      let rec (private_alias_of_js : Ojs.t -> private_alias) = alias_of_js
      and (private_alias_to_js : private_alias -> Ojs.t) = alias_to_js
      type record = {
        x: js ;
        y: js }
      let rec (record_of_js : Ojs.t -> record) =
        fun x124 ->
          {
            x = (js_of_js (Ojs.get_prop_ascii x124 "x"));
            y = (js_of_js (Ojs.get_prop_ascii x124 "y"))
          }
      and (record_to_js : record -> Ojs.t) =
        fun x123 ->
          Ojs.obj [|("x", (js_to_js x123.x));("y", (js_to_js x123.y))|]
      type mutable_record = {
        mutable x: js ;
        y: js }
      let rec (mutable_record_of_js : Ojs.t -> mutable_record) =
        fun x126 ->
          {
            x = (js_of_js (Ojs.get_prop_ascii x126 "x"));
            y = (js_of_js (Ojs.get_prop_ascii x126 "y"))
          }
      and (mutable_record_to_js : mutable_record -> Ojs.t) =
        fun x125 ->
          Ojs.obj [|("x", (js_to_js x125.x));("y", (js_to_js x125.y))|]
      type record_relabel = {
        x: int ;
        y: int }
      let rec (record_relabel_of_js : Ojs.t -> record_relabel) =
        fun x128 ->
          {
            x = (Ojs.int_of_js (Ojs.get_prop_ascii x128 "x"));
            y = (Ojs.int_of_js (Ojs.get_prop_ascii x128 "Y"))
          }
      and (record_relabel_to_js : record_relabel -> Ojs.t) =
        fun x127 ->
          Ojs.obj
            [|("x", (Ojs.int_to_js x127.x));("Y", (Ojs.int_to_js x127.y))|]
      type ('a, 'b) parametrized = {
        x: 'a ;
        y: 'b }
      let rec (parametrized_of_js :
        (Ojs.t -> 'a) -> (Ojs.t -> 'b) -> Ojs.t -> ('a, 'b) parametrized) =
        fun __a_of_js ->
          fun __b_of_js ->
            fun x130 ->
              {
                x = (__a_of_js (Ojs.get_prop_ascii x130 "x"));
                y = (__b_of_js (Ojs.get_prop_ascii x130 "y"))
              }
      and (parametrized_to_js :
        ('a -> Ojs.t) -> ('b -> Ojs.t) -> ('a, 'b) parametrized -> Ojs.t) =
        fun __a_to_js ->
          fun __b_to_js ->
            fun x129 ->
              Ojs.obj [|("x", (__a_to_js x129.x));("y", (__b_to_js x129.y))|]
      type specialized = (int, int) parametrized
      let rec (specialized_of_js : Ojs.t -> specialized) =
        fun x134 -> parametrized_of_js Ojs.int_of_js Ojs.int_of_js x134
      and (specialized_to_js : specialized -> Ojs.t) =
        fun x131 -> parametrized_to_js Ojs.int_to_js Ojs.int_to_js x131
      type enum =
        | Foo 
        | Bar 
        | Baz 
      let rec (enum_of_js : Ojs.t -> enum) =
        fun x138 ->
          let x139 = x138 in
          match Ojs.type_of x139 with
          | "number" ->
              (match Ojs.int_of_js x139 with | 42 -> Bar | _ -> assert false)
          | "string" ->
              (match Ojs.string_of_js x139 with
               | "foo" -> Foo
               | "Baz" -> Baz
               | _ -> assert false)
          | _ -> assert false
      and (enum_to_js : enum -> Ojs.t) =
        fun x137 ->
          match x137 with
          | Foo -> Ojs.string_to_js "foo"
          | Bar -> Ojs.int_to_js 42
          | Baz -> Ojs.string_to_js "Baz"
      type status =
        | OK 
        | KO 
        | OtherS of string 
        | OtherI of int 
      let rec (status_of_js : Ojs.t -> status) =
        fun x143 ->
          let x144 = x143 in
          match Ojs.type_of x144 with
          | "number" ->
              (match Ojs.int_of_js x144 with
               | 1 -> OK
               | 2 -> KO
               | x146 -> OtherI x146)
          | "string" ->
              (match Ojs.string_of_js x144 with | x145 -> OtherS x145)
          | _ -> assert false
      and (status_to_js : status -> Ojs.t) =
        fun x140 ->
          match x140 with
          | OK -> Ojs.int_to_js 1
          | KO -> Ojs.int_to_js 2
          | OtherS x141 -> Ojs.string_to_js x141
          | OtherI x142 -> Ojs.int_to_js x142
      type poly = [ `foo  | `bar  | `Baz  | `I of int  | `S of string ]
      let rec (poly_of_js : Ojs.t -> poly) =
        fun x150 ->
          let x151 = x150 in
          match Ojs.type_of x151 with
          | "number" ->
              (match Ojs.int_of_js x151 with | 42 -> `bar | x152 -> `I x152)
          | "string" ->
              (match Ojs.string_of_js x151 with
               | "foo" -> `foo
               | "Baz" -> `Baz
               | x153 -> `S x153)
          | _ -> assert false
      and (poly_to_js : poly -> Ojs.t) =
        fun x147 ->
          match x147 with
          | `foo -> Ojs.string_to_js "foo"
          | `bar -> Ojs.int_to_js 42
          | `Baz -> Ojs.string_to_js "Baz"
          | `I x148 -> Ojs.int_to_js x148
          | `S x149 -> Ojs.string_to_js x149
      type sum =
        | A 
        | B of int 
        | C of int * string 
        | D of {
        age: int ;
        name: string } 
        | Unknown of Ojs.t 
      let rec (sum_of_js : Ojs.t -> sum) =
        fun x161 ->
          let x162 = x161 in
          match Ojs.type_of (Ojs.get_prop_ascii x162 "kind") with
          | "number" ->
              (match Ojs.int_of_js (Ojs.get_prop_ascii x162 "kind") with
               | _ -> Unknown x162)
          | "string" ->
              (match Ojs.string_of_js (Ojs.get_prop_ascii x162 "kind") with
               | "A" -> A
               | "B" -> B (Ojs.int_of_js (Ojs.get_prop_ascii x162 "arg"))
               | "C" ->
                   C
                     ((Ojs.int_of_js
                         (Ojs.array_get (Ojs.get_prop_ascii x162 "arg") 0)),
                       (Ojs.string_of_js
                          (Ojs.array_get (Ojs.get_prop_ascii x162 "arg") 1)))
               | "D" ->
                   D
                     {
                       age = (Ojs.int_of_js (Ojs.get_prop_ascii x162 "age"));
                       name =
                         (Ojs.string_of_js (Ojs.get_prop_ascii x162 "name"))
                     }
               | _ -> Unknown x162)
          | "boolean" ->
              (match Ojs.bool_of_js (Ojs.get_prop_ascii x162 "kind") with
               | _ -> Unknown x162)
          | _ -> Unknown x162
      and (sum_to_js : sum -> Ojs.t) =
        fun x154 ->
          match x154 with
          | A -> Ojs.obj [|("kind", (Ojs.string_to_js "A"))|]
          | B x155 ->
              Ojs.obj
                [|("kind", (Ojs.string_to_js "B"));("arg",
                                                     (Ojs.int_to_js x155))|]
          | C (x156, x157) ->
              let x158 = Ojs.array_make 2 in
              (Ojs.array_set x158 1 (Ojs.string_to_js x157);
               Ojs.array_set x158 0 (Ojs.int_to_js x156);
               Ojs.obj [|("kind", (Ojs.string_to_js "C"));("arg", x158)|])
          | D x159 ->
              Ojs.obj
                [|("kind", (Ojs.string_to_js "D"));("age",
                                                     (Ojs.int_to_js x159.age));
                  ("name", (Ojs.string_to_js x159.name))|]
          | Unknown x160 ->
              Ojs.obj
                [|("kind", (Ojs.string_to_js "Unknown"));("arg", x160)|]
      type t =
        | A 
        | B of int 
        | C of int * string 
        | D of {
        age: int ;
        name: string } 
        | E of int 
        | Unknown of Ojs.t 
      let rec (t_of_js : Ojs.t -> t) =
        fun x171 ->
          let x172 = x171 in
          match Ojs.type_of (Ojs.get_prop_ascii x172 "kind") with
          | "number" ->
              (match Ojs.int_of_js (Ojs.get_prop_ascii x172 "kind") with
               | _ -> Unknown x172)
          | "string" ->
              (match Ojs.string_of_js (Ojs.get_prop_ascii x172 "kind") with
               | "A" -> A
               | "B" -> B (Ojs.int_of_js (Ojs.get_prop_ascii x172 "bArg"))
               | "C" ->
                   C
                     ((Ojs.int_of_js
                         (Ojs.array_get (Ojs.get_prop_ascii x172 "cArg") 0)),
                       (Ojs.string_of_js
                          (Ojs.array_get (Ojs.get_prop_ascii x172 "cArg") 1)))
               | "D" ->
                   D
                     {
                       age = (Ojs.int_of_js (Ojs.get_prop_ascii x172 "X"));
                       name =
                         (Ojs.string_of_js (Ojs.get_prop_ascii x172 "Y"))
                     }
               | "F" -> E (Ojs.int_of_js (Ojs.get_prop_ascii x172 "fArg"))
               | _ -> Unknown x172)
          | "boolean" ->
              (match Ojs.bool_of_js (Ojs.get_prop_ascii x172 "kind") with
               | _ -> Unknown x172)
          | _ -> Unknown x172
      and (t_to_js : t -> Ojs.t) =
        fun x163 ->
          match x163 with
          | A -> Ojs.obj [|("kind", (Ojs.string_to_js "A"))|]
          | B x164 ->
              Ojs.obj
                [|("kind", (Ojs.string_to_js "B"));("bArg",
                                                     (Ojs.int_to_js x164))|]
          | C (x165, x166) ->
              let x167 = Ojs.array_make 2 in
              (Ojs.array_set x167 1 (Ojs.string_to_js x166);
               Ojs.array_set x167 0 (Ojs.int_to_js x165);
               Ojs.obj [|("kind", (Ojs.string_to_js "C"));("cArg", x167)|])
          | D x168 ->
              Ojs.obj
                [|("kind", (Ojs.string_to_js "D"));("X",
                                                     (Ojs.int_to_js x168.age));
                  ("Y", (Ojs.string_to_js x168.name))|]
          | E x169 ->
              Ojs.obj
                [|("kind", (Ojs.string_to_js "F"));("fArg",
                                                     (Ojs.int_to_js x169))|]
          | Unknown x170 ->
              Ojs.obj
                [|("kind", (Ojs.string_to_js "Unknown"));("arg", x170)|]
      type union =
        | A 
        | B of int 
        | C of int 
        | D of Ojs.t 
      let rec (union_to_js : union -> Ojs.t) =
        fun x173 ->
          match x173 with
          | A -> Ojs.null
          | B x174 -> Ojs.int_to_js x174
          | C x175 -> Ojs.int_to_js x175
          | D x176 -> x176
      type poly_union = [ `A  | `B of int  | `C of int  | `D of Ojs.t ]
      let rec (poly_union_to_js : poly_union -> Ojs.t) =
        fun x179 ->
          match x179 with
          | `A -> Ojs.null
          | `B x180 -> Ojs.int_to_js x180
          | `C x181 -> Ojs.int_to_js x181
          | `D x182 -> x182
      type discr_union =
        | A 
        | B of int 
        | C of int 
        | D of Ojs.t 
      let rec (discr_union_of_js : Ojs.t -> discr_union) =
        fun x189 ->
          let x190 = x189 in
          match Ojs.type_of (Ojs.get_prop_ascii x190 "discr") with
          | "number" ->
              (match Ojs.int_of_js (Ojs.get_prop_ascii x190 "discr") with
               | _ -> D x190)
          | "string" ->
              (match Ojs.string_of_js (Ojs.get_prop_ascii x190 "discr") with
               | "A" -> A
               | "B" -> B (Ojs.int_of_js x190)
               | "C" -> C (Ojs.int_of_js x190)
               | _ -> D x190)
          | "boolean" ->
              (match Ojs.bool_of_js (Ojs.get_prop_ascii x190 "discr") with
               | _ -> D x190)
          | _ -> D x190
      and (discr_union_to_js : discr_union -> Ojs.t) =
        fun x185 ->
          match x185 with
          | A -> Ojs.null
          | B x186 -> Ojs.int_to_js x186
          | C x187 -> Ojs.int_to_js x187
          | D x188 -> x188
      type discr_poly_union = [ `A  | `B of int  | `C of int  | `D of Ojs.t ]
      let rec (discr_poly_union_of_js : Ojs.t -> discr_poly_union) =
        fun x195 ->
          let x196 = x195 in
          match Ojs.type_of (Ojs.get_prop_ascii x196 "discr") with
          | "number" ->
              (match Ojs.int_of_js (Ojs.get_prop_ascii x196 "discr") with
               | _ -> `D x196)
          | "string" ->
              (match Ojs.string_of_js (Ojs.get_prop_ascii x196 "discr") with
               | "A" -> `A
               | "B" -> `B (Ojs.int_of_js x196)
               | "C" -> `C (Ojs.int_of_js x196)
               | _ -> `D x196)
          | "boolean" ->
              (match Ojs.bool_of_js (Ojs.get_prop_ascii x196 "discr") with
               | _ -> `D x196)
          | _ -> `D x196
      and (discr_poly_union_to_js : discr_poly_union -> Ojs.t) =
        fun x191 ->
          match x191 with
          | `A -> Ojs.null
          | `B x192 -> Ojs.int_to_js x192
          | `C x193 -> Ojs.int_to_js x193
          | `D x194 -> x194
      type discr_union_value =
        | A 
        | B of int 
        | C of int 
        | D of Ojs.t 
      let rec (discr_union_value_of_js : Ojs.t -> discr_union_value) =
        fun x201 ->
          let x202 = x201 in
          match Ojs.type_of (Ojs.get_prop_ascii x202 "discr") with
          | "number" ->
              (match Ojs.int_of_js (Ojs.get_prop_ascii x202 "discr") with
               | 0 -> A
               | _ -> D x202)
          | "string" ->
              (match Ojs.string_of_js (Ojs.get_prop_ascii x202 "discr") with
               | "42" -> B (Ojs.int_of_js x202)
               | "C" -> C (Ojs.int_of_js x202)
               | _ -> D x202)
          | "boolean" ->
              (match Ojs.bool_of_js (Ojs.get_prop_ascii x202 "discr") with
               | _ -> D x202)
          | _ -> D x202
      and (discr_union_value_to_js : discr_union_value -> Ojs.t) =
        fun x197 ->
          match x197 with
          | A -> Ojs.null
          | B x198 -> Ojs.int_to_js x198
          | C x199 -> Ojs.int_to_js x199
          | D x200 -> x200
    end)[@merlin.hide ]) [@@ocaml.doc " Types Declarations "]
