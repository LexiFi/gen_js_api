open Node

(** Buffer **)

let caml_from_set s =
  let len = String.length s in
  let buf = Buffer.alloc len in
  String.iteri (fun k x ->
      Buffer.set buf k (Char.code x)
    ) s;
  buf

let caml_from_write s =
  let len = String.length s in
  let buf = Buffer.alloc len in
  let written = Buffer.write buf s in
  assert (written = len);
  buf

let assert_equal_buffer b1 b2 =
  let len1 = Buffer.length b1 in
  let len2 = Buffer.length b2 in
  assert (len1 = len2);
  for k = 0 to len1 -1 do
    assert (Buffer.get b1 k = Buffer.get b2 k)
  done

let copy src =
  let len = Buffer.length src in
  let dst = Buffer.alloc len in
  let written =
    Buffer.copy src ~dst ~start:0 ~dst_start:0 ~dst_end:len
  in
  assert (len = written);
  dst

let () =
  let test = "test" in
  let native = Buffer.from test in
  let from_set = caml_from_set test in
  let from_write = caml_from_write test in
  let from_copy = copy native in
  assert_equal_buffer native from_set;
  assert_equal_buffer native from_write;
  assert_equal_buffer native from_copy

(** Path **)

let () = assert (Path.sep = Filename.dir_sep)

(** Process **)

let () =
  Container.StringMap.iter
    (fun key value ->
       assert (Sys.getenv key = value)
    )
    Process.env

let uncaught_handler p =
  let open Promise in
  catch p (fun error ->
      Printf.eprintf "Uncaught execption: %s\n" (Printexc.to_string (Obj.magic error));
      exit 1
    )

(** FileSystem **)

let root : unit Promise.t =
  let open Promise in
  uncaught_handler
    begin
      let* contents = Fs.readdir "."
      and+ contents' = Fs.readdir "." in
      List.iter2 (fun x y ->
          assert (x = y)) contents contents';
      return ()
    end

(*** Index signature **)

include [%js:
  module MapLike : sig
    type 'a t = private Ojs.t

    val t_to_js: ('a -> Ojs.t) -> 'a t -> Ojs.t
    val t_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a t

    val get: 'a t -> string -> 'a option [@@js.index_get]
    val set: 'a t -> string -> 'a -> unit [@@js.index_set]
  end

  type js_string = private Ojs.t
  val js_string_of_js: Ojs.t -> js_string
  val js_string_to_js: js_string -> Ojs.t

  [@@@js.stop]
  val js_string: string -> js_string
  [@@@js.start]
  [@@@js.implem
    let js_string = Ojs.string_to_js
  ]
]

let () =
  let test_obj = Ojs.obj [| ("foo", Ojs.string_to_js "bar") |] in
  let map_str = MapLike.t_of_js (js_string_of_js) test_obj in
  assert (MapLike.get map_str "foo" = Some (js_string "bar"));
  MapLike.set map_str "baz" (js_string "boo");
  assert (MapLike.get map_str "baz" = Some (js_string "boo"));
  ()

(*** Function signature **)

include [%js:
  module Concat : sig
    type t = private Ojs.t

    val t_to_js: t -> Ojs.t
    val t_of_js: Ojs.t -> t

    val apply: t -> (string list [@js.variadic]) -> string [@@js.apply]
  end

  module [@js.scope Imports.path] Path2 : sig
    val join: Concat.t [@@js.global "join"]
  end
]

let () =
  let args = ["foo"; "bar"; "baz"] in
  let res1 = Path.join args in
  let res2 = Concat.apply Path2.join args in
  assert (res1 = res2);
  ()

(** Arrays **)
let () =
    let open Arrays.StringArray in
    let a = create () in
    for k = 0 to 10 do
      push a (string_of_int k);
    done;
    let s = join a "," in
    List.iteri (fun k x ->
      assert (string_of_int k = x)
    ) (String.split_on_char ',' s)
