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

(*** FileSystem **)

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
