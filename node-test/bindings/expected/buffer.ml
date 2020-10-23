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
let (write : t -> string -> unit) =
  fun x12 ->
    fun x11 ->
      ignore (Ojs.call (t_to_js x12) "write" [|(Ojs.string_to_js x11)|])
let (slice : t -> int -> int -> t) =
  fun x15 ->
    fun x13 ->
      fun x14 ->
        t_of_js
          (Ojs.call (t_to_js x15) "slice"
             [|(Ojs.int_to_js x13);(Ojs.int_to_js x14)|])
let (to_string : t -> string) =
  fun x16 -> Ojs.string_of_js (Ojs.get (t_to_js x16) "toString")
let (copy :
  src:t ->
    dst:t -> ?src_start:int -> ?dst_start:int -> ?dst_end:int -> unit -> int)
  =
  fun ~src:x17 ->
    fun ~dst:x18 ->
      fun ?src_start:x19 ->
        fun ?dst_start:x20 ->
          fun ?dst_end:x21 ->
            fun () ->
              Ojs.int_of_js
                (let x26 = Ojs.get Ojs.global "Buffer" in
                 Ojs.call (Ojs.get x26 "copy") "apply"
                   [|x26;((let x22 =
                             Ojs.new_obj (Ojs.get Ojs.global "Array") [||] in
                           ignore (Ojs.call x22 "push" [|(t_to_js x17)|]);
                           ignore (Ojs.call x22 "push" [|(t_to_js x18)|]);
                           (match x19 with
                            | Some x25 ->
                                ignore
                                  (Ojs.call x22 "push"
                                     [|(Ojs.int_to_js x25)|])
                            | None -> ());
                           (match x20 with
                            | Some x24 ->
                                ignore
                                  (Ojs.call x22 "push"
                                     [|(Ojs.int_to_js x24)|])
                            | None -> ());
                           (match x21 with
                            | Some x23 ->
                                ignore
                                  (Ojs.call x22 "push"
                                     [|(Ojs.int_to_js x23)|])
                            | None -> ());
                           x22))|])
