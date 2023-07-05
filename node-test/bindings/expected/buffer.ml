[@@@js.dummy "!! This code has been generated by gen_js_api !!"]
[@@@ocaml.warning "-7-32-39"]
type t = Ojs.t
let rec t_of_js : Ojs.t -> t = fun (x2 : Ojs.t) -> x2
and t_to_js : t -> Ojs.t = fun (x1 : Ojs.t) -> x1
let (alloc : int -> t) =
  fun (x3 : int) ->
    t_of_js
      (Jsoo_runtime.Js.meth_call
         (Jsoo_runtime.Js.get (Jsoo_runtime.Js.pure_js_expr "globalThis")
            (Obj.magic "Buffer")) "alloc" [|(Ojs.int_to_js x3)|])
let (from : string -> t) =
  fun (x4 : string) ->
    t_of_js
      (Jsoo_runtime.Js.meth_call
         (Jsoo_runtime.Js.get (Jsoo_runtime.Js.pure_js_expr "globalThis")
            (Obj.magic "Buffer")) "from" [|(Ojs.string_to_js x4)|])
let (concat : t list -> t) =
  fun (x5 : t list) ->
    t_of_js
      (Jsoo_runtime.Js.meth_call
         (Jsoo_runtime.Js.get (Jsoo_runtime.Js.pure_js_expr "globalThis")
            (Obj.magic "Buffer")) "concat" [|(Ojs.list_to_js t_to_js x5)|])
let (length : t -> int) =
  fun (x7 : t) ->
    Ojs.int_of_js (Jsoo_runtime.Js.get (t_to_js x7) (Obj.magic "length"))
let (get : t -> int -> int option) =
  fun (x8 : t) ->
    fun (x9 : int) ->
      Ojs.option_of_js Ojs.int_of_js
        (Jsoo_runtime.Js.get (t_to_js x8) (Ojs.int_to_js x9))
let (set : t -> int -> int -> unit) =
  fun (x11 : t) ->
    fun (x12 : int) ->
      fun (x13 : int) ->
        Jsoo_runtime.Js.set (t_to_js x11) (Ojs.int_to_js x12)
          (Ojs.int_to_js x13)
let (write : t -> string -> int) =
  fun (x15 : t) ->
    fun (x14 : string) ->
      Ojs.int_of_js
        (Jsoo_runtime.Js.meth_call (t_to_js x15) "write"
           [|(Ojs.string_to_js x14)|])
let (slice : t -> int -> int -> t) =
  fun (x18 : t) ->
    fun (x16 : int) ->
      fun (x17 : int) ->
        t_of_js
          (Jsoo_runtime.Js.meth_call (t_to_js x18) "slice"
             [|(Ojs.int_to_js x16);(Ojs.int_to_js x17)|])
let (to_string : t -> string) =
  fun (x19 : t) ->
    Ojs.string_of_js
      (Jsoo_runtime.Js.meth_call (t_to_js x19) "toString" [||])
let (copy : t -> dst:t -> start:int -> dst_start:int -> dst_end:int -> int) =
  fun (x24 : t) ->
    fun ~dst:(x20 : t) ->
      fun ~start:(x21 : int) ->
        fun ~dst_start:(x22 : int) ->
          fun ~dst_end:(x23 : int) ->
            Ojs.int_of_js
              (Jsoo_runtime.Js.meth_call (t_to_js x24) "copy"
                 [|(t_to_js x20);(Ojs.int_to_js x21);(Ojs.int_to_js x22);(
                   Ojs.int_to_js x23)|])
