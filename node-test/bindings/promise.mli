module UntypedPromise : sig

  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t

  [@@@js.stop]
    val return: Ojs.t -> t
    val fail: Ojs.t -> t
    val bind: ?error:(Ojs.t -> t) -> t -> (Ojs.t -> t) -> t
    val all: Ojs.t list -> t
  [@@@js.start]

  [@@@js.implem
    val resolve: Ojs.t -> Ojs.t [@@js.global "Promise.resolve"]
    val reject: Ojs.t -> Ojs.t [@@js.global "Promise.reject"]
    val then_: Ojs.t -> success:(Ojs.t -> Ojs.t) -> error:(Ojs.t -> Ojs.t) -> Ojs.t [@@js.call "then"]
    val all: Ojs.t list -> Ojs.t [@@js.global "Promise.all"]

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

    let return x = resolve (wrap x)
    let fail err = reject (wrap err)
    let bind ?(error = fail) p f =
      then_ p ~success:(fun x -> f (unwrap x))
              ~error:(fun x -> error (unwrap x))

  ]
end

  [@@@js.stop]
  type 'a t = UntypedPromise.t
  val t_to_js: ('a -> Ojs.t) -> 'a t -> Ojs.t
  val t_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a t

  type error = Ojs.t

  val fail: error -> 'a t
  val return: 'a -> 'a t
  val bind: ?error:(error -> 'b t) -> 'a t -> ('a -> 'b t) -> 'b t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val prod: 'a t -> 'b t -> ('a * 'b) t

  val ( let+ ): 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ): 'a t -> 'b t -> ('a * 'b) t
  val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
  val ( and* ): 'a t -> 'b t -> ('a * 'b) t

  [@@@js.start]
  [@@@js.implem
    type 'a t = UntypedPromise.t
    type error = Ojs.t
    let fail error = UntypedPromise.fail  error
    let return x = UntypedPromise.return (Obj.magic x)
    let bind ?error p f =
      UntypedPromise.bind ?error p
        (fun x -> f (Obj.magic x))

    let prod p1 p2 =
      bind (UntypedPromise.all [p1; p2])
        (fun ojs ->
          match Ojs.list_of_js Ojs.t_of_js ojs with
          | [x1; x2] ->
            UntypedPromise.return ([%js.of: Ojs.t * Ojs.t] (x1, x2))
          | _ -> assert false
        )
    let map f p = bind p (fun x -> return (f x))
    let t_to_js f p = UntypedPromise.t_to_js (map f p)
    let t_of_js f p = map f (UntypedPromise.t_of_js p)

    let (let+) p f = map f p
    let (and+) = prod
    let (let*) p f = bind p f
    let (and*) = prod

  ]