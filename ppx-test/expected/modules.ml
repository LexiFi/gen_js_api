[@@@js.dummy "!! This code has been generated by gen_js_api !!"]
[@@@ocaml.warning "-7-32-39"]
module Event =
  struct
    type t = Ojs.t
    let rec (t_of_js : Ojs.t -> t) = fun (x2 : Ojs.t) -> x2
    and (t_to_js : t -> Ojs.t) = fun (x1 : Ojs.t) -> x1
  end
module Foo =
  struct
    module E = Event
    let (foo : E.t -> string -> unit) =
      fun (x4 : E.t) ->
        fun (x3 : string) ->
          ignore (Ojs.call (E.t_to_js x4) "foo" [|(Ojs.string_to_js x3)|])
  end
module Bar =
  struct
    include Event
    let (bar : t -> string -> unit) =
      fun (x6 : t) ->
        fun (x5 : string) ->
          ignore (Ojs.call (t_to_js x6) "bar" [|(Ojs.string_to_js x5)|])
  end
