module UntypedPromise : sig

  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t

  [@@@js.stop]
    val return: Ojs.t -> t
    val bind: t -> (Ojs.t -> t) -> t
  [@@@js.start]

  [@@@js.implem
    val resolve: Ojs.t -> Ojs.t [@@js.global "Promise.resolve"]
    val then_: Ojs.t -> (Ojs.t -> Ojs.t) -> Ojs.t [@@js.call "then"]

    type wrap = { content: Ojs.t }[@@js]

    let is_promise o =
      resolve o == o

    let wrap o =
      if is_promise o then
        wrap_to_js { content = o }
      else o

    let unwrap o =
      if Ojs.has_property o "content" then
        Ojs.get o "content"
      else
        o

    let return x =
      resolve (wrap x)

    let bind p f =
      then_ p (fun x -> f (unwrap x))

  ]
end

  [@@@js.stop]
  type 'a t = UntypedPromise.t
  val t_to_js: ('a -> Ojs.t) -> 'a t -> Ojs.t
  val t_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a t

  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  [@@@js.start]
  [@@@js.implem
    type 'a t = UntypedPromise.t
    let return x = UntypedPromise.return (Obj.magic x)
    let bind p f = UntypedPromise.bind p (fun x -> f (Obj.magic x))
    let map f p = bind p (fun x -> return (f x))
    let t_to_js f p = UntypedPromise.t_to_js (map f p)
    let t_of_js f p = map f (UntypedPromise.t_of_js p)
  ]
