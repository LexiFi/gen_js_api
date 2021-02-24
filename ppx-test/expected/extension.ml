let _ = Ojs.int_to_js
let _ =
  fun (x2 : int -> int) ->
    Ojs.fun_to_js 1
      (fun (x3 : Ojs.t) -> Ojs.int_to_js (x2 (Ojs.int_of_js x3)))
