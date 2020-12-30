[@@@js.dummy "!! This code has been generated by gen_js_api !!"]
[@@@ocaml.warning "-7-32-39"]
type t = Ojs.t
let rec (t_of_js : Ojs.t -> t) = fun x2 -> x2
and (t_to_js : t -> Ojs.t) = fun x1 -> x1
let (alloc : int -> t) =
  fun x3 ->
    t_of_js
      (Ojs.call (Ojs.get Ojs.global "Buffer") "alloc" [|(Ojs.int_to_js x3)|])
let (from : string -> t) =
  fun x4 ->
    t_of_js
      (Ojs.call (Ojs.get Ojs.global "Buffer") "from"
         [|(Ojs.string_to_js x4)|])
let (concat : t list -> t) =
  fun x5 ->
    t_of_js
      (Ojs.call (Ojs.get Ojs.global "Buffer") "concat"
         [|(Ojs.list_to_js t_to_js x5)|])
let (length : t -> int) =
  fun x7 -> Ojs.int_of_js (Ojs.get (t_to_js x7) "length")
let get buf index =
  (fun x8 -> Ojs.option_of_js Ojs.int_of_js x8) (Ojs.array_get buf index)
let set buf index value = Ojs.array_set buf index (Ojs.int_to_js value)
let (write : t -> string -> int) =
  fun x12 ->
    fun x11 ->
      Ojs.int_of_js
        (Ojs.call (t_to_js x12) "write" [|(Ojs.string_to_js x11)|])
let (slice : t -> int -> int -> t) =
  fun x15 ->
    fun x13 ->
      fun x14 ->
        t_of_js
          (Ojs.call (t_to_js x15) "slice"
             [|(Ojs.int_to_js x13);(Ojs.int_to_js x14)|])
let (to_string : t -> string) =
  fun x16 -> Ojs.string_of_js (Ojs.call (t_to_js x16) "toString" [||])
let (copy : t -> dst:t -> start:int -> dst_start:int -> dst_end:int -> int) =
  fun x21 ->
    fun ~dst:x17 ->
      fun ~start:x18 ->
        fun ~dst_start:x19 ->
          fun ~dst_end:x20 ->
            Ojs.int_of_js
              (Ojs.call (t_to_js x21) "copy"
                 [|(t_to_js x17);(Ojs.int_to_js x18);(Ojs.int_to_js x19);(
                   Ojs.int_to_js x20)|])