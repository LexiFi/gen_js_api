(***************************************************************************)
(*  Copyright (C) 2000-2015 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

(* $Id: ojs.ml 80187 2015-03-27 16:40:57Z afrisch $ *)

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
