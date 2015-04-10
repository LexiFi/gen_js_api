(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by Alain Frisch and LexiFi.                             *)

(* This module (mostly) abstracts away from js_of_ocaml encoding of
   OCaml values.  It serves as a support library for the code generated
   by gen_js_api.

   It would be quite easy to completely drop dependency to
   js_of_ocaml's library (so as to only rely on its compiler and JS
   runtime code).
*)


type t = Js.Unsafe.any

let t_to_js x = x
let t_of_js x = x

let string_of_js o = Js.to_string (Obj.magic o)
let string_to_js s = Js.Unsafe.inject (Js.string s)

let int_of_js = Obj.magic
let int_to_js = Js.Unsafe.inject

let bool_of_js o = Js.to_bool (Obj.magic o)
let bool_to_js x = Js.Unsafe.inject (Js.bool x)

let float_of_js = Obj.magic
let float_to_js = Js.Unsafe.inject

let fun_to_js f = Js.Unsafe.inject (Js.wrap_callback f)

let call = Js.Unsafe.meth_call
let call_unit o s args = ignore (call o s args)

let apply f x = Js.Unsafe.fun_call f x
let apply_unit f x = ignore (apply f x)

let get = Js.Unsafe.get
let set = Js.Unsafe.set

let obj = Js.Unsafe.obj


let array_get t i = Obj.magic (Js.array_get (Obj.magic t) i)

let array_set t i x = Js.array_set (Obj.magic t) i x


let array_of_js f objs =
  Array.init
    (int_of_js (get objs "length"))
    (fun i -> f (array_get objs i))

let array_to_js f arr =
  let n = Array.length arr in
  let a = Js.Unsafe.new_obj Js.array_length [|int_to_js n|] in
  for i = 0 to n - 1 do
    array_set a i (f arr.(i))
  done;
  a

let list_of_js f objs =
  Array.to_list (array_of_js f objs)

let list_to_js f l =
  array_to_js f (Array.of_list l)

let option_of_js f x =
  if Js.Opt.test (Obj.magic x) && Js.Optdef.test (Obj.magic x) then
    Some (f x)
  else
    None

let option_to_js f = function
  | Some x -> f x
  | None -> Obj.magic (Js.null)


let variable = Js.Unsafe.variable

let new_obj name args =
  let constr = Js.Unsafe.get Js.Unsafe.global name in
  Js.Unsafe.new_obj constr args

let type_of x = Js.to_string (Js.typeof (Obj.magic x))
