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
let (_ : Ojs.t of_js) = fun (x9 : Ojs.t) -> x9
let (_ : Ojs.t to_js) = fun (x10 : Ojs.t) -> x10
let (_ : (string * int) of_js) =
  fun (x11 : Ojs.t) ->
    let x12 = x11 in
    ((Ojs.string_of_js (Ojs.array_get x12 0)),
      (Ojs.int_of_js (Ojs.array_get x12 1)))
let (_ : (string * int) to_js) =
  fun (x13 : (string * int)) ->
    let (x14, x15) = x13 in
    let x16 = Ojs.array_make 2 in
    Ojs.array_set x16 0 (Ojs.string_to_js x14);
    Ojs.array_set x16 1 (Ojs.int_to_js x15);
    x16
let (_ : (string * int * bool) of_js) =
  fun (x17 : Ojs.t) ->
    let x18 = x17 in
    ((Ojs.string_of_js (Ojs.array_get x18 0)),
      (Ojs.int_of_js (Ojs.array_get x18 1)),
      (Ojs.bool_of_js (Ojs.array_get x18 2)))
let (_ : (string * int * bool) to_js) =
  fun (x19 : (string * int * bool)) ->
    let (x20, x21, x22) = x19 in
    let x23 = Ojs.array_make 3 in
    Ojs.array_set x23 0 (Ojs.string_to_js x20);
    Ojs.array_set x23 1 (Ojs.int_to_js x21);
    Ojs.array_set x23 2 (Ojs.bool_to_js x22);
    x23
let (_ : (string -> int) of_js) =
  fun (x24 : Ojs.t) ->
    fun (x25 : string) ->
      Ojs.int_of_js (Ojs.apply x24 [|(Ojs.string_to_js x25)|])
let (_ : (string -> int) to_js) =
  fun (x26 : string -> int) ->
    Ojs.fun_to_js 1
      (fun (x27 : Ojs.t) -> Ojs.int_to_js (x26 (Ojs.string_of_js x27)))
let (_ : ((string -> int) -> bool -> unit) of_js) =
  fun (x28 : Ojs.t) ->
    fun (x29 : string -> int) ->
      fun (x31 : bool) ->
        ignore
          (Ojs.apply x28
             [|(Ojs.fun_to_js 1
                  (fun (x30 : Ojs.t) ->
                     Ojs.int_to_js (x29 (Ojs.string_of_js x30))));(Ojs.bool_to_js
                                                                    x31)|])
let (_ : ((string -> int) -> bool -> unit) to_js) =
  fun (x32 : (string -> int) -> bool -> unit) ->
    Ojs.fun_to_js 2
      (fun (x33 : Ojs.t) ->
         fun (x35 : Ojs.t) ->
           x32
             (fun (x34 : string) ->
                Ojs.int_of_js (Ojs.apply x33 [|(Ojs.string_to_js x34)|]))
             (Ojs.bool_of_js x35))
let (_ : string array of_js) =
  fun (x36 : Ojs.t) -> Ojs.array_of_js Ojs.string_of_js x36
let (_ : string array to_js) =
  fun (x38 : string array) -> Ojs.array_to_js Ojs.string_to_js x38
let (_ : string list of_js) =
  fun (x40 : Ojs.t) -> Ojs.list_of_js Ojs.string_of_js x40
let (_ : string list to_js) =
  fun (x42 : string list) -> Ojs.list_to_js Ojs.string_to_js x42
let (_ : string option of_js) =
  fun (x44 : Ojs.t) -> Ojs.option_of_js Ojs.string_of_js x44
let (_ : string option to_js) =
  fun (x46 : string option) -> Ojs.option_to_js Ojs.string_to_js x46
let (_ : (_ -> _) of_js) =
  fun (x48 : Ojs.t) ->
    fun (x49 : 'a) -> Obj.magic (Ojs.apply x48 [|(Obj.magic x49)|])
let (_ : (_ -> _) to_js) =
  fun (x50 : 'a -> 'b) ->
    Ojs.fun_to_js 1 (fun (x51 : Ojs.t) -> Obj.magic (x50 (Obj.magic x51)))
let (_ : [ `foo  | `bar  | `Baz  | `I of int  | `S of string ] of_js) =
  fun (x52 : Ojs.t) ->
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
  fun (x56 : [ `foo  | `bar  | `Baz  | `I of int  | `S of string ]) ->
    match x56 with
    | `foo -> Ojs.string_to_js "foo"
    | `bar -> Ojs.int_to_js 42
    | `Baz -> Ojs.string_to_js "Baz"
    | `I x57 -> Ojs.int_to_js x57
    | `S x58 -> Ojs.string_to_js x58
[@@@ocaml.text " Label & Options Value "]
let (_ : (label:int -> ?opt:int -> unit -> unit) of_js) =
  fun (x59 : Ojs.t) ->
    fun ~label:(x60 : int) ->
      fun ?opt:(x61 : int option) ->
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
  fun (x64 : label:int -> ?opt:int -> unit -> unit) ->
    Ojs.fun_to_js 2
      (fun (x65 : Ojs.t) ->
         fun (x66 : Ojs.t) ->
           x64 ~label:(Ojs.int_of_js x65)
             ?opt:(Ojs.option_of_js Ojs.int_of_js x66) ())
let (_ : (label:int -> ?opt:int -> unit -> unit) of_js) =
  fun (x68 : Ojs.t) ->
    fun ~label:(x69 : int) ->
      fun ?opt:(x70 : int option) ->
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
  fun (x73 : label:int -> ?opt:int -> unit -> unit) ->
    Ojs.fun_to_js 2
      (fun (x74 : Ojs.t) ->
         fun (x75 : Ojs.t) ->
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
        fun ?x:(x77 : int option) ->
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
        fun ?x:(x81 : int option) ->
          fun () ->
            ignore
              (Ojs.call Ojs.global "default1"
                 [|(Ojs.int_to_js
                      (match x81 with | Some x82 -> x82 | None -> 42))|])
      let (builder0 : unit -> Ojs.t) =
        fun () -> let x83 = Ojs.empty_obj () in x83
      let (builder1 : x:int -> Ojs.t) =
        fun ~x:(x84 : int) ->
          let x85 = Ojs.empty_obj () in
          Ojs.set_prop_ascii x85 "x" (Ojs.int_to_js x84); x85
      let (builder2 : ?x:int -> ?y:string -> unit -> Ojs.t) =
        fun ?x:(x86 : int option) ->
          fun ?y:(x87 : string option) ->
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
        fun ~x:(x91 : int) ->
          fun ~y:(x92 : string) ->
            fun () ->
              let x93 = Ojs.empty_obj () in
              Ojs.set_prop_ascii x93 "x" (Ojs.int_to_js x91);
              Ojs.set_prop_ascii x93 "y" (Ojs.string_to_js x92);
              x93
      let (builder4 : x:int -> y:string -> z:unit -> Ojs.t) =
        fun ~x:(x94 : int) ->
          fun ~y:(x95 : string) ->
            fun ~z:(x96 : unit) ->
              let x97 = Ojs.empty_obj () in
              Ojs.set_prop_ascii x97 "x" (Ojs.int_to_js x94);
              Ojs.set_prop_ascii x97 "y" (Ojs.string_to_js x95);
              Ojs.set_prop_ascii x97 "z" (Ojs.unit_to_js x96);
              x97
      let (builder5 : ?x:int -> ?y:string -> unit -> Ojs.t) =
        fun ?x:(x98 : int option) ->
          fun ?y:(x99 : string option) ->
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
        fun ?x:(x103 : int option) ->
          fun ?y:(x104 : string option) ->
            fun ?z:(x105 : int option) ->
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
        fun (x110 : string) ->
          fun (x111 : string list) ->
            Ojs.string_of_js
              (let x114 = Ojs.global in
               Ojs.call (Ojs.get_prop_ascii x114 "sep") "apply"
                 [|x114;((let x112 =
                            Ojs.new_obj
                              (Ojs.get_prop_ascii Ojs.global "Array") 
                              [||] in
                          ignore
                            (Ojs.call x112 "push" [|(Ojs.string_to_js x110)|]);
                          List.iter
                            (fun (x113 : string) ->
                               ignore
                                 (Ojs.call x112 "push"
                                    [|(Ojs.string_to_js x113)|])) x111;
                          x112))|])
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
    type 'a abs = ('a -> int) -> unit
    type specialized = (int, int) parametrized
    type enum =
      | Foo 
      | Bar 
      | Baz 
      | Qux 
    type status =
      | OK 
      | KO 
      | OO 
      | OtherS of string 
      | OtherI of int 
    type poly = [ `foo  | `bar  | `baz  | `Qux  | `I of int  | `S of string ]
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
      let rec js_of_js : Ojs.t -> js = fun (x116 : Ojs.t) -> x116
      and js_to_js : js -> Ojs.t = fun (x115 : Ojs.t) -> x115
      type abstract = Ojs.t
      let rec abstract_of_js : Ojs.t -> abstract = fun (x118 : Ojs.t) -> x118
      and abstract_to_js : abstract -> Ojs.t = fun (x117 : Ojs.t) -> x117
      type alias = js
      let rec alias_of_js : Ojs.t -> alias =
        fun (x120 : Ojs.t) -> js_of_js x120
      and alias_to_js : alias -> Ojs.t = fun (x119 : js) -> js_to_js x119
      type private_alias = alias
      let rec private_alias_of_js : Ojs.t -> private_alias =
        fun (x122 : Ojs.t) -> alias_of_js x122
      and private_alias_to_js : private_alias -> Ojs.t =
        fun (x121 : alias) -> alias_to_js x121
      type record = {
        x: js ;
        y: js }
      let rec record_of_js : Ojs.t -> record =
        fun (x124 : Ojs.t) ->
          {
            x = (js_of_js (Ojs.get_prop_ascii x124 "x"));
            y = (js_of_js (Ojs.get_prop_ascii x124 "y"))
          }
      and record_to_js : record -> Ojs.t =
        fun (x123 : record) ->
          Ojs.obj [|("x", (js_to_js x123.x));("y", (js_to_js x123.y))|]
      type mutable_record = {
        mutable x: js ;
        y: js }
      let rec mutable_record_of_js : Ojs.t -> mutable_record =
        fun (x126 : Ojs.t) ->
          {
            x = (js_of_js (Ojs.get_prop_ascii x126 "x"));
            y = (js_of_js (Ojs.get_prop_ascii x126 "y"))
          }
      and mutable_record_to_js : mutable_record -> Ojs.t =
        fun (x125 : mutable_record) ->
          Ojs.obj [|("x", (js_to_js x125.x));("y", (js_to_js x125.y))|]
      type record_relabel = {
        x: int ;
        y: int }
      let rec record_relabel_of_js : Ojs.t -> record_relabel =
        fun (x128 : Ojs.t) ->
          {
            x = (Ojs.int_of_js (Ojs.get_prop_ascii x128 "x"));
            y = (Ojs.int_of_js (Ojs.get_prop_ascii x128 "Y"))
          }
      and record_relabel_to_js : record_relabel -> Ojs.t =
        fun (x127 : record_relabel) ->
          Ojs.obj
            [|("x", (Ojs.int_to_js x127.x));("Y", (Ojs.int_to_js x127.y))|]
      type ('a, 'b) parametrized = {
        x: 'a ;
        y: 'b }
      let rec parametrized_of_js :
        'a 'b .
          (Ojs.t -> 'a) -> (Ojs.t -> 'b) -> Ojs.t -> ('a, 'b) parametrized
        = fun (type __a) -> fun (type __b) ->
        fun (__a_of_js : Ojs.t -> __a) ->
          fun (__b_of_js : Ojs.t -> __b) ->
            fun (x130 : Ojs.t) ->
              {
                x = (__a_of_js (Ojs.get_prop_ascii x130 "x"));
                y = (__b_of_js (Ojs.get_prop_ascii x130 "y"))
              }
      and parametrized_to_js :
        'a 'b .
          ('a -> Ojs.t) -> ('b -> Ojs.t) -> ('a, 'b) parametrized -> Ojs.t
        = fun (type __a) -> fun (type __b) ->
        fun (__a_to_js : __a -> Ojs.t) ->
          fun (__b_to_js : __b -> Ojs.t) ->
            fun (x129 : (__a, __b) parametrized) ->
              Ojs.obj [|("x", (__a_to_js x129.x));("y", (__b_to_js x129.y))|]
      type 'a abs = ('a -> int) -> unit
      let rec abs_of_js : 'a . (Ojs.t -> 'a) -> Ojs.t -> 'a abs = fun (type
        __a) ->
        fun (__a_of_js : Ojs.t -> __a) ->
          fun (x134 : Ojs.t) ->
            fun (x135 : __a -> int) ->
              ignore
                (Ojs.apply x134
                   [|(Ojs.fun_to_js 1
                        (fun (x136 : Ojs.t) ->
                           Ojs.int_to_js (x135 (__a_of_js x136))))|])
      and abs_to_js : 'a . ('a -> Ojs.t) -> 'a abs -> Ojs.t = fun (type __a)
        ->
        fun (__a_to_js : __a -> Ojs.t) ->
          fun (x131 : (__a -> int) -> unit) ->
            Ojs.fun_to_js 1
              (fun (x132 : Ojs.t) ->
                 x131
                   (fun (x133 : __a) ->
                      Ojs.int_of_js (Ojs.apply x132 [|(__a_to_js x133)|])))
      type specialized = (int, int) parametrized
      let rec specialized_of_js : Ojs.t -> specialized =
        fun (x140 : Ojs.t) ->
          parametrized_of_js Ojs.int_of_js Ojs.int_of_js x140
      and specialized_to_js : specialized -> Ojs.t =
        fun (x137 : (int, int) parametrized) ->
          parametrized_to_js Ojs.int_to_js Ojs.int_to_js x137
      type enum =
        | Foo 
        | Bar 
        | Baz 
        | Qux 
      let rec enum_of_js : Ojs.t -> enum =
        fun (x144 : Ojs.t) ->
          let x145 = x144 in
          match Ojs.type_of x145 with
          | "number" ->
              (match Ojs.float_of_js x145 with
               | 4.2 -> Baz
               | _ ->
                   (match Ojs.int_of_js x145 with
                    | 42 -> Bar
                    | _ -> assert false))
          | "string" ->
              (match Ojs.string_of_js x145 with
               | "foo" -> Foo
               | "Qux" -> Qux
               | _ -> assert false)
          | _ -> assert false
      and enum_to_js : enum -> Ojs.t =
        fun (x143 : enum) ->
          match x143 with
          | Foo -> Ojs.string_to_js "foo"
          | Bar -> Ojs.int_to_js 42
          | Baz -> Ojs.float_to_js 4.2
          | Qux -> Ojs.string_to_js "Qux"
      type status =
        | OK 
        | KO 
        | OO 
        | OtherS of string 
        | OtherI of int 
      let rec status_of_js : Ojs.t -> status =
        fun (x149 : Ojs.t) ->
          let x150 = x149 in
          match Ojs.type_of x150 with
          | "number" ->
              (match Ojs.float_of_js x150 with
               | 1.5 -> OO
               | _ ->
                   (match Ojs.int_of_js x150 with
                    | 1 -> OK
                    | 2 -> KO
                    | x152 -> OtherI x152))
          | "string" ->
              (match Ojs.string_of_js x150 with | x151 -> OtherS x151)
          | _ -> assert false
      and status_to_js : status -> Ojs.t =
        fun (x146 : status) ->
          match x146 with
          | OK -> Ojs.int_to_js 1
          | KO -> Ojs.int_to_js 2
          | OO -> Ojs.float_to_js 1.5
          | OtherS x147 -> Ojs.string_to_js x147
          | OtherI x148 -> Ojs.int_to_js x148
      type poly =
        [ `foo  | `bar  | `baz  | `Qux  | `I of int  | `S of string ]
      let rec poly_of_js : Ojs.t -> poly =
        fun (x156 : Ojs.t) ->
          let x157 = x156 in
          match Ojs.type_of x157 with
          | "number" ->
              (match Ojs.float_of_js x157 with
               | 4.2 -> `baz
               | _ ->
                   (match Ojs.int_of_js x157 with
                    | 42 -> `bar
                    | x158 -> `I x158))
          | "string" ->
              (match Ojs.string_of_js x157 with
               | "foo" -> `foo
               | "Qux" -> `Qux
               | x159 -> `S x159)
          | _ -> assert false
      and poly_to_js : poly -> Ojs.t =
        fun
          (x153 :
            [ `foo  | `bar  | `baz  | `Qux  | `I of int  | `S of string ])
          ->
          match x153 with
          | `foo -> Ojs.string_to_js "foo"
          | `bar -> Ojs.int_to_js 42
          | `baz -> Ojs.float_to_js 4.2
          | `Qux -> Ojs.string_to_js "Qux"
          | `I x154 -> Ojs.int_to_js x154
          | `S x155 -> Ojs.string_to_js x155
      type sum =
        | A 
        | B of int 
        | C of int * string 
        | D of {
        age: int ;
        name: string } 
        | Unknown of Ojs.t 
      let rec sum_of_js : Ojs.t -> sum =
        fun (x167 : Ojs.t) ->
          let x168 = x167 in
          match Ojs.type_of (Ojs.get_prop_ascii x168 "kind") with
          | "number" -> Unknown x168
          | "string" ->
              (match Ojs.string_of_js (Ojs.get_prop_ascii x168 "kind") with
               | "A" -> A
               | "B" -> B (Ojs.int_of_js (Ojs.get_prop_ascii x168 "arg"))
               | "C" ->
                   C
                     ((Ojs.int_of_js
                         (Ojs.array_get (Ojs.get_prop_ascii x168 "arg") 0)),
                       (Ojs.string_of_js
                          (Ojs.array_get (Ojs.get_prop_ascii x168 "arg") 1)))
               | "D" ->
                   D
                     {
                       age = (Ojs.int_of_js (Ojs.get_prop_ascii x168 "age"));
                       name =
                         (Ojs.string_of_js (Ojs.get_prop_ascii x168 "name"))
                     }
               | _ -> Unknown x168)
          | "boolean" -> Unknown x168
          | _ -> Unknown x168
      and sum_to_js : sum -> Ojs.t =
        fun (x160 : sum) ->
          match x160 with
          | A -> Ojs.obj [|("kind", (Ojs.string_to_js "A"))|]
          | B x161 ->
              Ojs.obj
                [|("kind", (Ojs.string_to_js "B"));("arg",
                                                     (Ojs.int_to_js x161))|]
          | C (x162, x163) ->
              let x164 = Ojs.array_make 2 in
              (Ojs.array_set x164 1 (Ojs.string_to_js x163);
               Ojs.array_set x164 0 (Ojs.int_to_js x162);
               Ojs.obj [|("kind", (Ojs.string_to_js "C"));("arg", x164)|])
          | D x165 ->
              Ojs.obj
                [|("kind", (Ojs.string_to_js "D"));("age",
                                                     (Ojs.int_to_js x165.age));
                  ("name", (Ojs.string_to_js x165.name))|]
          | Unknown x166 ->
              Ojs.obj
                [|("kind", (Ojs.string_to_js "Unknown"));("arg", x166)|]
      type t =
        | A 
        | B of int 
        | C of int * string 
        | D of {
        age: int ;
        name: string } 
        | E of int 
        | Unknown of Ojs.t 
      let rec t_of_js : Ojs.t -> t =
        fun (x177 : Ojs.t) ->
          let x178 = x177 in
          match Ojs.type_of (Ojs.get_prop_ascii x178 "kind") with
          | "number" -> Unknown x178
          | "string" ->
              (match Ojs.string_of_js (Ojs.get_prop_ascii x178 "kind") with
               | "A" -> A
               | "B" -> B (Ojs.int_of_js (Ojs.get_prop_ascii x178 "bArg"))
               | "C" ->
                   C
                     ((Ojs.int_of_js
                         (Ojs.array_get (Ojs.get_prop_ascii x178 "cArg") 0)),
                       (Ojs.string_of_js
                          (Ojs.array_get (Ojs.get_prop_ascii x178 "cArg") 1)))
               | "D" ->
                   D
                     {
                       age = (Ojs.int_of_js (Ojs.get_prop_ascii x178 "X"));
                       name =
                         (Ojs.string_of_js (Ojs.get_prop_ascii x178 "Y"))
                     }
               | "F" -> E (Ojs.int_of_js (Ojs.get_prop_ascii x178 "fArg"))
               | _ -> Unknown x178)
          | "boolean" -> Unknown x178
          | _ -> Unknown x178
      and t_to_js : t -> Ojs.t =
        fun (x169 : t) ->
          match x169 with
          | A -> Ojs.obj [|("kind", (Ojs.string_to_js "A"))|]
          | B x170 ->
              Ojs.obj
                [|("kind", (Ojs.string_to_js "B"));("bArg",
                                                     (Ojs.int_to_js x170))|]
          | C (x171, x172) ->
              let x173 = Ojs.array_make 2 in
              (Ojs.array_set x173 1 (Ojs.string_to_js x172);
               Ojs.array_set x173 0 (Ojs.int_to_js x171);
               Ojs.obj [|("kind", (Ojs.string_to_js "C"));("cArg", x173)|])
          | D x174 ->
              Ojs.obj
                [|("kind", (Ojs.string_to_js "D"));("X",
                                                     (Ojs.int_to_js x174.age));
                  ("Y", (Ojs.string_to_js x174.name))|]
          | E x175 ->
              Ojs.obj
                [|("kind", (Ojs.string_to_js "F"));("fArg",
                                                     (Ojs.int_to_js x175))|]
          | Unknown x176 ->
              Ojs.obj
                [|("kind", (Ojs.string_to_js "Unknown"));("arg", x176)|]
      type union =
        | A 
        | B of int 
        | C of int 
        | D of Ojs.t 
      let rec union_to_js : union -> Ojs.t =
        fun (x179 : union) ->
          match x179 with
          | A -> Ojs.null
          | B x180 -> Ojs.int_to_js x180
          | C x181 -> Ojs.int_to_js x181
          | D x182 -> x182
      type poly_union = [ `A  | `B of int  | `C of int  | `D of Ojs.t ]
      let rec poly_union_to_js : poly_union -> Ojs.t =
        fun (x185 : [ `A  | `B of int  | `C of int  | `D of Ojs.t ]) ->
          match x185 with
          | `A -> Ojs.null
          | `B x186 -> Ojs.int_to_js x186
          | `C x187 -> Ojs.int_to_js x187
          | `D x188 -> x188
      type discr_union =
        | A 
        | B of int 
        | C of int 
        | D of Ojs.t 
      let rec discr_union_of_js : Ojs.t -> discr_union =
        fun (x195 : Ojs.t) ->
          let x196 = x195 in
          match Ojs.type_of (Ojs.get_prop_ascii x196 "discr") with
          | "number" -> D x196
          | "string" ->
              (match Ojs.string_of_js (Ojs.get_prop_ascii x196 "discr") with
               | "A" -> A
               | "B" -> B (Ojs.int_of_js x196)
               | "C" -> C (Ojs.int_of_js x196)
               | _ -> D x196)
          | "boolean" -> D x196
          | _ -> D x196
      and discr_union_to_js : discr_union -> Ojs.t =
        fun (x191 : discr_union) ->
          match x191 with
          | A -> Ojs.null
          | B x192 -> Ojs.int_to_js x192
          | C x193 -> Ojs.int_to_js x193
          | D x194 -> x194
      type discr_poly_union = [ `A  | `B of int  | `C of int  | `D of Ojs.t ]
      let rec discr_poly_union_of_js : Ojs.t -> discr_poly_union =
        fun (x201 : Ojs.t) ->
          let x202 = x201 in
          match Ojs.type_of (Ojs.get_prop_ascii x202 "discr") with
          | "number" -> `D x202
          | "string" ->
              (match Ojs.string_of_js (Ojs.get_prop_ascii x202 "discr") with
               | "A" -> `A
               | "B" -> `B (Ojs.int_of_js x202)
               | "C" -> `C (Ojs.int_of_js x202)
               | _ -> `D x202)
          | "boolean" -> `D x202
          | _ -> `D x202
      and discr_poly_union_to_js : discr_poly_union -> Ojs.t =
        fun (x197 : [ `A  | `B of int  | `C of int  | `D of Ojs.t ]) ->
          match x197 with
          | `A -> Ojs.null
          | `B x198 -> Ojs.int_to_js x198
          | `C x199 -> Ojs.int_to_js x199
          | `D x200 -> x200
      type discr_union_value =
        | A 
        | B of int 
        | C of int 
        | D of Ojs.t 
      let rec discr_union_value_of_js : Ojs.t -> discr_union_value =
        fun (x207 : Ojs.t) ->
          let x208 = x207 in
          match Ojs.type_of (Ojs.get_prop_ascii x208 "discr") with
          | "number" ->
              (match Ojs.int_of_js (Ojs.get_prop_ascii x208 "discr") with
               | 0 -> A
               | _ -> D x208)
          | "string" ->
              (match Ojs.string_of_js (Ojs.get_prop_ascii x208 "discr") with
               | "42" -> B (Ojs.int_of_js x208)
               | "C" -> C (Ojs.int_of_js x208)
               | _ -> D x208)
          | "boolean" -> D x208
          | _ -> D x208
      and discr_union_value_to_js : discr_union_value -> Ojs.t =
        fun (x203 : discr_union_value) ->
          match x203 with
          | A -> Ojs.null
          | B x204 -> Ojs.int_to_js x204
          | C x205 -> Ojs.int_to_js x205
          | D x206 -> x206
    end)[@merlin.hide ]) [@@ocaml.doc " Types Declarations "]
