type enum_int =
  | Enum_int_0 [@js 0]
  | Enum_int_1 [@js 1]
  | Enum_int_other of int [@js.default]
  [@@js.enum]

type enum_float =
  | Enum_float_0_1 [@js 0.1]
  | Enum_float_1_1 [@js 1.1]
  | Enum_float_other of float [@js.default]
  [@@js.enum]

(* float cases should be matched first *)
type enum_number_1 =
  | Enum_number_0 [@js 0]
  | Enum_number_1 [@js 1]
  | Enum_number_0_1 [@js 0.1]
  | Enum_number_1_1 [@js 1.1]
  | Enum_number_other of int [@js.default]
  [@@js.enum]

(* float cases should be matched first even if the default case is float *)
type enum_number_2 =
  | Enum_number_0 [@js 0]
  | Enum_number_1 [@js 1]
  | Enum_number_0_1 [@js 0.1]
  | Enum_number_1_1 [@js 1.1]
  | Enum_number_other of float [@js.default]
  [@@js.enum]

type enum_string =
  | Enum_string_foo [@js "foo"]
  | Enum_string_bar [@js "bar"]
  | Enum_string_other of string [@js.default]
  [@@js.enum]

(* if both true and false are expected, the boolean part of `_of_js` should not have the default case *)
type enum_bool =
  | Enum_bool_true [@js true]
  | Enum_bool_false [@js false]
  [@@js.enum]

(* otherwise, an unknown boolean value should trigger `assert false` *)
type enum_bool_partial =
  | Enum_bool_true [@js true]
  [@@js.enum]

(* or it should be mapped to the case with `js.default` *)
type enum_bool_partial2 =
  | Enum_bool_true [@js true]
  | Enum_bool_other of bool [@js.default]
  [@@js.enum]

(* if both true and false are expected, the boolean part of `_of_js` should not have the default case *)
type enum_mixed =
  | Enum_int_0 [@js 0]
  | Enum_int_1 [@js 1]
  | Enum_float_0_1 [@js 0.1]
  | Enum_float_1_1 [@js 1.1]
  | Enum_number_other of int [@js.default]
  | Enum_string_foo [@js "foo"]
  | Enum_string_bar [@js "bar"]
  | Enum_string_other of string [@js.default]
  | Enum_bool_true [@js true]
  | Enum_bool_false [@js false]
  [@@js.enum]

(* otherwise, an unknown boolean value should trigger `assert false` *)
type enum_mixed_partial_bool =
  | Enum_int_0 [@js 0]
  | Enum_int_1 [@js 1]
  | Enum_float_0_1 [@js 0.1]
  | Enum_float_1_1 [@js 1.1]
  | Enum_number_other of float [@js.default]
  | Enum_string_foo [@js "foo"]
  | Enum_string_bar [@js "bar"]
  | Enum_string_other of string [@js.default]
  | Enum_bool_true [@js true]
  [@@js.enum]

(* or it should be mapped to the case with `js.default` *)
type enum_mixed_partial_bool2 =
  | Enum_int_0 [@js 0]
  | Enum_int_1 [@js 1]
  | Enum_float_0_1 [@js 0.1]
  | Enum_float_1_1 [@js 1.1]
  | Enum_number_other of float [@js.default]
  | Enum_string_foo [@js "foo"]
  | Enum_string_bar [@js "bar"]
  | Enum_string_other of string [@js.default]
  | Enum_bool_true [@js true]
  | Enum_bool_other of bool [@js.default]
  [@@js.enum]

type dummy1
type dummy2
type dummy3
type dummy4
type dummy5
type dummy6

type union_int =
  | Union_int_0 of dummy1 [@js 0]
  | Union_int_1 of dummy2 [@js 1]
  | Unknown of Ojs.t [@js.default]
  [@@js.union on_field "tag"]

type union_float =
  | Union_float_0_1 of dummy1 [@js 0.1]
  | Union_float_1_1 of dummy2 [@js 1.1]
  | Unknown of Ojs.t [@js.default]
  [@@js.union on_field "tag"]

type union_string =
  | Union_string_foo of dummy3 [@js "foo"]
  | Union_string_bar of dummy4 [@js "bar"]
  | Unknown of Ojs.t [@js.default]
  [@@js.union on_field "tag"]

(* if both true and false are expected, the boolean part of `_of_js` should not have the default case *)
type union_bool =
  | Union_bool_true of dummy5 [@js true]
  | Union_bool_false of dummy6 [@js false]
  [@@js.union on_field "tag"]

(* otherwise, an unknown boolean value should trigger `assert false` *)
type union_bool_partial =
  | Union_bool_true of dummy5 [@js true]
  [@@js.union on_field "tag"]

(* or it should be mapped to `Unknown` *)
type union_bool_partial2 =
  | Union_bool_true of dummy5 [@js true]
  | Unknown of Ojs.t [@js.default]
  [@@js.union on_field "tag"]

(* if both true and false are expected, the boolean part of `_of_js` should not have the default case *)
type union_mixed =
  | Union_int_0 of dummy1 [@js 0]
  | Union_int_1 of dummy2 [@js 1]
  | Union_float_0_1 of dummy1 [@js 0.1]
  | Union_float_1_1 of dummy2 [@js 1.1]
  | Union_string_foo of dummy3 [@js "foo"]
  | Union_string_bar of dummy4 [@js "bar"]
  | Union_bool_true of dummy5 [@js true]
  | Union_bool_false of dummy6 [@js false]
  | Unknown of Ojs.t [@js.default]
  [@@js.union on_field "tag"]

(* otherwise, an unknown boolean value should be mapped to `Unknown` *)
type union_mixed_partial_bool =
  | Union_int_0 of dummy1 [@js 0]
  | Union_int_1 of dummy2 [@js 1]
  | Union_float_0_1 of dummy1 [@js 0.1]
  | Union_float_1_1 of dummy2 [@js 1.1]
  | Union_string_foo of dummy3 [@js "foo"]
  | Union_string_bar of dummy4 [@js "bar"]
  | Union_bool_true of dummy5 [@js true]
  | Unknown of Ojs.t [@js.default]
  [@@js.union on_field "tag"]