[@@@js.dummy "!! This code has been generated by gen_js_api !!"]
[@@@ocaml.warning "-7-32-39"]
type enum_int =
  | Enum_int_0 
  | Enum_int_1 
  | Enum_int_other of int 
let rec enum_int_of_js : Ojs.t -> enum_int =
  fun (x3 : Ojs.t) ->
    let x4 = x3 in
    match Ojs.int_of_js x4 with
    | 0 -> Enum_int_0
    | 1 -> Enum_int_1
    | x5 -> Enum_int_other x5
and enum_int_to_js : enum_int -> Ojs.t =
  fun (x1 : enum_int) ->
    match x1 with
    | Enum_int_0 -> Ojs.int_to_js 0
    | Enum_int_1 -> Ojs.int_to_js 1
    | Enum_int_other x2 -> Ojs.int_to_js x2
type enum_string =
  | Enum_string_foo 
  | Enum_string_bar 
  | Enum_string_other of string 
let rec enum_string_of_js : Ojs.t -> enum_string =
  fun (x8 : Ojs.t) ->
    let x9 = x8 in
    match Ojs.string_of_js x9 with
    | "foo" -> Enum_string_foo
    | "bar" -> Enum_string_bar
    | x10 -> Enum_string_other x10
and enum_string_to_js : enum_string -> Ojs.t =
  fun (x6 : enum_string) ->
    match x6 with
    | Enum_string_foo -> Ojs.string_to_js "foo"
    | Enum_string_bar -> Ojs.string_to_js "bar"
    | Enum_string_other x7 -> Ojs.string_to_js x7
type enum_bool =
  | Enum_bool_true 
  | Enum_bool_false 
let rec enum_bool_of_js : Ojs.t -> enum_bool =
  fun (x12 : Ojs.t) ->
    let x13 = x12 in
    match Ojs.bool_of_js x13 with
    | true -> Enum_bool_true
    | false -> Enum_bool_false
and enum_bool_to_js : enum_bool -> Ojs.t =
  fun (x11 : enum_bool) ->
    match x11 with
    | Enum_bool_true -> Ojs.bool_to_js true
    | Enum_bool_false -> Ojs.bool_to_js false
type enum_bool_partial =
  | Enum_bool_true 
let rec enum_bool_partial_of_js : Ojs.t -> enum_bool_partial =
  fun (x15 : Ojs.t) ->
    let x16 = x15 in
    match Ojs.bool_of_js x16 with
    | true -> Enum_bool_true
    | _ -> assert false
and enum_bool_partial_to_js : enum_bool_partial -> Ojs.t =
  fun (x14 : enum_bool_partial) ->
    match x14 with | Enum_bool_true -> Ojs.bool_to_js true
type enum_bool_partial2 =
  | Enum_bool_true 
  | Enum_bool_other of bool 
let rec enum_bool_partial2_of_js : Ojs.t -> enum_bool_partial2 =
  fun (x19 : Ojs.t) ->
    let x20 = x19 in
    match Ojs.bool_of_js x20 with
    | true -> Enum_bool_true
    | x21 -> Enum_bool_other x21
and enum_bool_partial2_to_js : enum_bool_partial2 -> Ojs.t =
  fun (x17 : enum_bool_partial2) ->
    match x17 with
    | Enum_bool_true -> Ojs.bool_to_js true
    | Enum_bool_other x18 -> Ojs.bool_to_js x18
type enum_mixed =
  | Enum_int_0 
  | Enum_int_1 
  | Enum_int_other of int 
  | Enum_string_foo 
  | Enum_string_bar 
  | Enum_string_other of string 
  | Enum_bool_true 
  | Enum_bool_false 
let rec enum_mixed_of_js : Ojs.t -> enum_mixed =
  fun (x25 : Ojs.t) ->
    let x26 = x25 in
    match Ojs.type_of x26 with
    | "number" ->
        (match Ojs.int_of_js x26 with
         | 0 -> Enum_int_0
         | 1 -> Enum_int_1
         | x27 -> Enum_int_other x27)
    | "string" ->
        (match Ojs.string_of_js x26 with
         | "foo" -> Enum_string_foo
         | "bar" -> Enum_string_bar
         | x28 -> Enum_string_other x28)
    | "boolean" ->
        (match Ojs.bool_of_js x26 with
         | true -> Enum_bool_true
         | false -> Enum_bool_false)
    | _ -> assert false
and enum_mixed_to_js : enum_mixed -> Ojs.t =
  fun (x22 : enum_mixed) ->
    match x22 with
    | Enum_int_0 -> Ojs.int_to_js 0
    | Enum_int_1 -> Ojs.int_to_js 1
    | Enum_int_other x23 -> Ojs.int_to_js x23
    | Enum_string_foo -> Ojs.string_to_js "foo"
    | Enum_string_bar -> Ojs.string_to_js "bar"
    | Enum_string_other x24 -> Ojs.string_to_js x24
    | Enum_bool_true -> Ojs.bool_to_js true
    | Enum_bool_false -> Ojs.bool_to_js false
type enum_mixed_partial_bool =
  | Enum_int_0 
  | Enum_int_1 
  | Enum_int_other of int 
  | Enum_string_foo 
  | Enum_string_bar 
  | Enum_string_other of string 
  | Enum_bool_true 
let rec enum_mixed_partial_bool_of_js : Ojs.t -> enum_mixed_partial_bool =
  fun (x32 : Ojs.t) ->
    let x33 = x32 in
    match Ojs.type_of x33 with
    | "number" ->
        (match Ojs.int_of_js x33 with
         | 0 -> Enum_int_0
         | 1 -> Enum_int_1
         | x34 -> Enum_int_other x34)
    | "string" ->
        (match Ojs.string_of_js x33 with
         | "foo" -> Enum_string_foo
         | "bar" -> Enum_string_bar
         | x35 -> Enum_string_other x35)
    | "boolean" ->
        (match Ojs.bool_of_js x33 with
         | true -> Enum_bool_true
         | _ -> assert false)
    | _ -> assert false
and enum_mixed_partial_bool_to_js : enum_mixed_partial_bool -> Ojs.t =
  fun (x29 : enum_mixed_partial_bool) ->
    match x29 with
    | Enum_int_0 -> Ojs.int_to_js 0
    | Enum_int_1 -> Ojs.int_to_js 1
    | Enum_int_other x30 -> Ojs.int_to_js x30
    | Enum_string_foo -> Ojs.string_to_js "foo"
    | Enum_string_bar -> Ojs.string_to_js "bar"
    | Enum_string_other x31 -> Ojs.string_to_js x31
    | Enum_bool_true -> Ojs.bool_to_js true
type enum_mixed_partial_bool2 =
  | Enum_int_0 
  | Enum_int_1 
  | Enum_int_other of int 
  | Enum_string_foo 
  | Enum_string_bar 
  | Enum_string_other of string 
  | Enum_bool_true 
  | Enum_bool_other of bool 
let rec enum_mixed_partial_bool2_of_js : Ojs.t -> enum_mixed_partial_bool2 =
  fun (x40 : Ojs.t) ->
    let x41 = x40 in
    match Ojs.type_of x41 with
    | "number" ->
        (match Ojs.int_of_js x41 with
         | 0 -> Enum_int_0
         | 1 -> Enum_int_1
         | x42 -> Enum_int_other x42)
    | "string" ->
        (match Ojs.string_of_js x41 with
         | "foo" -> Enum_string_foo
         | "bar" -> Enum_string_bar
         | x43 -> Enum_string_other x43)
    | "boolean" ->
        (match Ojs.bool_of_js x41 with
         | true -> Enum_bool_true
         | x44 -> Enum_bool_other x44)
    | _ -> assert false
and enum_mixed_partial_bool2_to_js : enum_mixed_partial_bool2 -> Ojs.t =
  fun (x36 : enum_mixed_partial_bool2) ->
    match x36 with
    | Enum_int_0 -> Ojs.int_to_js 0
    | Enum_int_1 -> Ojs.int_to_js 1
    | Enum_int_other x37 -> Ojs.int_to_js x37
    | Enum_string_foo -> Ojs.string_to_js "foo"
    | Enum_string_bar -> Ojs.string_to_js "bar"
    | Enum_string_other x38 -> Ojs.string_to_js x38
    | Enum_bool_true -> Ojs.bool_to_js true
    | Enum_bool_other x39 -> Ojs.bool_to_js x39
type dummy1 = Ojs.t
let rec dummy1_of_js : Ojs.t -> dummy1 = fun (x46 : Ojs.t) -> x46
and dummy1_to_js : dummy1 -> Ojs.t = fun (x45 : Ojs.t) -> x45
type dummy2 = Ojs.t
let rec dummy2_of_js : Ojs.t -> dummy2 = fun (x48 : Ojs.t) -> x48
and dummy2_to_js : dummy2 -> Ojs.t = fun (x47 : Ojs.t) -> x47
type dummy3 = Ojs.t
let rec dummy3_of_js : Ojs.t -> dummy3 = fun (x50 : Ojs.t) -> x50
and dummy3_to_js : dummy3 -> Ojs.t = fun (x49 : Ojs.t) -> x49
type dummy4 = Ojs.t
let rec dummy4_of_js : Ojs.t -> dummy4 = fun (x52 : Ojs.t) -> x52
and dummy4_to_js : dummy4 -> Ojs.t = fun (x51 : Ojs.t) -> x51
type dummy5 = Ojs.t
let rec dummy5_of_js : Ojs.t -> dummy5 = fun (x54 : Ojs.t) -> x54
and dummy5_to_js : dummy5 -> Ojs.t = fun (x53 : Ojs.t) -> x53
type dummy6 = Ojs.t
let rec dummy6_of_js : Ojs.t -> dummy6 = fun (x56 : Ojs.t) -> x56
and dummy6_to_js : dummy6 -> Ojs.t = fun (x55 : Ojs.t) -> x55
type union_int =
  | Union_int_0 of dummy1 
  | Union_int_1 of dummy2 
  | Unknown of Ojs.t 
let rec union_int_of_js : Ojs.t -> union_int =
  fun (x61 : Ojs.t) ->
    let x62 = x61 in
    match Ojs.type_of (Ojs.get_prop_ascii x62 "tag") with
    | "number" ->
        (match Ojs.int_of_js (Ojs.get_prop_ascii x62 "tag") with
         | 0 -> Union_int_0 (dummy1_of_js x62)
         | 1 -> Union_int_1 (dummy2_of_js x62)
         | _ -> Unknown x62)
    | "string" ->
        (match Ojs.string_of_js (Ojs.get_prop_ascii x62 "tag") with
         | _ -> Unknown x62)
    | "boolean" ->
        (match Ojs.bool_of_js (Ojs.get_prop_ascii x62 "tag") with
         | _ -> Unknown x62)
    | _ -> Unknown x62
and union_int_to_js : union_int -> Ojs.t =
  fun (x57 : union_int) ->
    match x57 with
    | Union_int_0 x58 -> dummy1_to_js x58
    | Union_int_1 x59 -> dummy2_to_js x59
    | Unknown x60 -> x60
type union_string =
  | Union_string_foo of dummy3 
  | Union_string_bar of dummy4 
  | Unknown of Ojs.t 
let rec union_string_of_js : Ojs.t -> union_string =
  fun (x67 : Ojs.t) ->
    let x68 = x67 in
    match Ojs.type_of (Ojs.get_prop_ascii x68 "tag") with
    | "number" ->
        (match Ojs.int_of_js (Ojs.get_prop_ascii x68 "tag") with
         | _ -> Unknown x68)
    | "string" ->
        (match Ojs.string_of_js (Ojs.get_prop_ascii x68 "tag") with
         | "foo" -> Union_string_foo (dummy3_of_js x68)
         | "bar" -> Union_string_bar (dummy4_of_js x68)
         | _ -> Unknown x68)
    | "boolean" ->
        (match Ojs.bool_of_js (Ojs.get_prop_ascii x68 "tag") with
         | _ -> Unknown x68)
    | _ -> Unknown x68
and union_string_to_js : union_string -> Ojs.t =
  fun (x63 : union_string) ->
    match x63 with
    | Union_string_foo x64 -> dummy3_to_js x64
    | Union_string_bar x65 -> dummy4_to_js x65
    | Unknown x66 -> x66
type union_bool =
  | Union_bool_true of dummy5 
  | Union_bool_false of dummy6 
let rec union_bool_of_js : Ojs.t -> union_bool =
  fun (x72 : Ojs.t) ->
    let x73 = x72 in
    match Ojs.bool_of_js (Ojs.get_prop_ascii x73 "tag") with
    | true -> Union_bool_true (dummy5_of_js x73)
    | false -> Union_bool_false (dummy6_of_js x73)
and union_bool_to_js : union_bool -> Ojs.t =
  fun (x69 : union_bool) ->
    match x69 with
    | Union_bool_true x70 -> dummy5_to_js x70
    | Union_bool_false x71 -> dummy6_to_js x71
type union_bool_partial =
  | Union_bool_true of dummy5 
let rec union_bool_partial_of_js : Ojs.t -> union_bool_partial =
  fun (x76 : Ojs.t) ->
    let x77 = x76 in
    match Ojs.bool_of_js (Ojs.get_prop_ascii x77 "tag") with
    | true -> Union_bool_true (dummy5_of_js x77)
    | _ -> assert false
and union_bool_partial_to_js : union_bool_partial -> Ojs.t =
  fun (x74 : union_bool_partial) ->
    match x74 with | Union_bool_true x75 -> dummy5_to_js x75
type union_bool_partial2 =
  | Union_bool_true of dummy5 
  | Unknown of Ojs.t 
let rec union_bool_partial2_of_js : Ojs.t -> union_bool_partial2 =
  fun (x81 : Ojs.t) ->
    let x82 = x81 in
    match Ojs.type_of (Ojs.get_prop_ascii x82 "tag") with
    | "number" ->
        (match Ojs.int_of_js (Ojs.get_prop_ascii x82 "tag") with
         | _ -> Unknown x82)
    | "string" ->
        (match Ojs.string_of_js (Ojs.get_prop_ascii x82 "tag") with
         | _ -> Unknown x82)
    | "boolean" ->
        (match Ojs.bool_of_js (Ojs.get_prop_ascii x82 "tag") with
         | true -> Union_bool_true (dummy5_of_js x82)
         | _ -> Unknown x82)
    | _ -> Unknown x82
and union_bool_partial2_to_js : union_bool_partial2 -> Ojs.t =
  fun (x78 : union_bool_partial2) ->
    match x78 with
    | Union_bool_true x79 -> dummy5_to_js x79
    | Unknown x80 -> x80
type union_mixed =
  | Union_int_0 of dummy1 
  | Union_int_1 of dummy2 
  | Union_string_foo of dummy3 
  | Union_string_bar of dummy4 
  | Union_bool_true of dummy5 
  | Union_bool_false of dummy6 
  | Unknown of Ojs.t 
let rec union_mixed_of_js : Ojs.t -> union_mixed =
  fun (x91 : Ojs.t) ->
    let x92 = x91 in
    match Ojs.type_of (Ojs.get_prop_ascii x92 "tag") with
    | "number" ->
        (match Ojs.int_of_js (Ojs.get_prop_ascii x92 "tag") with
         | 0 -> Union_int_0 (dummy1_of_js x92)
         | 1 -> Union_int_1 (dummy2_of_js x92)
         | _ -> Unknown x92)
    | "string" ->
        (match Ojs.string_of_js (Ojs.get_prop_ascii x92 "tag") with
         | "foo" -> Union_string_foo (dummy3_of_js x92)
         | "bar" -> Union_string_bar (dummy4_of_js x92)
         | _ -> Unknown x92)
    | "boolean" ->
        (match Ojs.bool_of_js (Ojs.get_prop_ascii x92 "tag") with
         | true -> Union_bool_true (dummy5_of_js x92)
         | false -> Union_bool_false (dummy6_of_js x92))
    | _ -> Unknown x92
and union_mixed_to_js : union_mixed -> Ojs.t =
  fun (x83 : union_mixed) ->
    match x83 with
    | Union_int_0 x84 -> dummy1_to_js x84
    | Union_int_1 x85 -> dummy2_to_js x85
    | Union_string_foo x86 -> dummy3_to_js x86
    | Union_string_bar x87 -> dummy4_to_js x87
    | Union_bool_true x88 -> dummy5_to_js x88
    | Union_bool_false x89 -> dummy6_to_js x89
    | Unknown x90 -> x90
type union_mixed_partial_bool =
  | Union_int_0 of dummy1 
  | Union_int_1 of dummy2 
  | Union_string_foo of dummy3 
  | Union_string_bar of dummy4 
  | Union_bool_true of dummy5 
  | Unknown of Ojs.t 
let rec union_mixed_partial_bool_of_js : Ojs.t -> union_mixed_partial_bool =
  fun (x100 : Ojs.t) ->
    let x101 = x100 in
    match Ojs.type_of (Ojs.get_prop_ascii x101 "tag") with
    | "number" ->
        (match Ojs.int_of_js (Ojs.get_prop_ascii x101 "tag") with
         | 0 -> Union_int_0 (dummy1_of_js x101)
         | 1 -> Union_int_1 (dummy2_of_js x101)
         | _ -> Unknown x101)
    | "string" ->
        (match Ojs.string_of_js (Ojs.get_prop_ascii x101 "tag") with
         | "foo" -> Union_string_foo (dummy3_of_js x101)
         | "bar" -> Union_string_bar (dummy4_of_js x101)
         | _ -> Unknown x101)
    | "boolean" ->
        (match Ojs.bool_of_js (Ojs.get_prop_ascii x101 "tag") with
         | true -> Union_bool_true (dummy5_of_js x101)
         | _ -> Unknown x101)
    | _ -> Unknown x101
and union_mixed_partial_bool_to_js : union_mixed_partial_bool -> Ojs.t =
  fun (x93 : union_mixed_partial_bool) ->
    match x93 with
    | Union_int_0 x94 -> dummy1_to_js x94
    | Union_int_1 x95 -> dummy2_to_js x95
    | Union_string_foo x96 -> dummy3_to_js x96
    | Union_string_bar x97 -> dummy4_to_js x97
    | Union_bool_true x98 -> dummy5_to_js x98
    | Unknown x99 -> x99
