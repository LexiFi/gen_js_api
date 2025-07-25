[@@@js.dummy "!! This code has been generated by gen_js_api !!"]
[@@@ocaml.warning "-7-32-39"]
module UntypedPromise =
  struct
    type t = Ojs.t
    let rec (t_of_js : Ojs.t -> t) = fun (x2 : Ojs.t) -> x2
    and (t_to_js : t -> Ojs.t) = fun (x1 : Ojs.t) -> x1
    let (resolve : Ojs.t -> Ojs.t) =
      fun (x3 : Ojs.t) ->
        Ojs.call (Ojs.get_prop_ascii Ojs.global "Promise") "resolve" [|x3|]
    let (reject : Ojs.t -> Ojs.t) =
      fun (x4 : Ojs.t) ->
        Ojs.call (Ojs.get_prop_ascii Ojs.global "Promise") "reject" [|x4|]
    let (then_ :
      Ojs.t -> success:(Ojs.t -> Ojs.t) -> error:(Ojs.t -> Ojs.t) -> Ojs.t) =
      fun (x9 : Ojs.t) ->
        fun ~success:(x5 : Ojs.t -> Ojs.t) ->
          fun ~error:(x7 : Ojs.t -> Ojs.t) ->
            Ojs.call x9 "then" [|(Ojs.fun_to_js 1 x5);(Ojs.fun_to_js 1 x7)|]
    let (all : Ojs.t list -> Ojs.t) =
      fun (x10 : Ojs.t list) ->
        Ojs.call (Ojs.get_prop_ascii Ojs.global "Promise") "all"
          [|(Ojs.list_to_js (fun (x11 : Ojs.t) -> x11) x10)|]
    include
      struct
        type wrap = {
          content: Ojs.t }
        [@@@ocaml.warning "-7-32-39"]
        let rec (wrap_of_js : Ojs.t -> wrap) =
          fun (x13 : Ojs.t) ->
            { content = (Ojs.get_prop_ascii x13 "content") }
        and (wrap_to_js : wrap -> Ojs.t) =
          fun (x12 : wrap) -> Ojs.obj [|("content", (x12.content))|]
      end
    let is_promise o = (resolve o) == o
    let wrap o = if is_promise o then wrap_to_js { content = o } else o
    let unwrap o =
      if Ojs.has_property o "content"
      then Ojs.get_prop_ascii o "content"
      else o
    let return x = resolve (wrap x)
    let fail err = reject (wrap err)
    let bind ?(error= fail)  p f =
      then_ p ~success:(fun x -> f (unwrap x))
        ~error:(fun x -> error (unwrap x))
  end
type 'a t = UntypedPromise.t
type error = Ojs.t
let fail error = UntypedPromise.fail error
let return x = UntypedPromise.return (Obj.magic x)
let bind ?error  p f =
  UntypedPromise.bind ?error p (fun x -> f (Obj.magic x))
let prod p1 p2 =
  bind (UntypedPromise.all [p1; p2])
    (fun ojs ->
       match Ojs.list_of_js Ojs.t_of_js ojs with
       | x1::x2::[] -> return (x1, x2)
       | _ -> assert false)
let map f p = bind p (fun x -> return (f x))
let t_to_js f p = UntypedPromise.t_to_js (map f p)
let t_of_js f p = map f (UntypedPromise.t_of_js p)
let (let+) p f = map f p
let (and+) = prod
let ( let* ) p f = bind p f
let ( and* ) = prod
let catch p error = bind p ~error return
