open Node

let () = assert (Path.sep = Filename.dir_sep)

let () =
 Container.StringMap.iter
  (fun key value ->
    assert (Sys.getenv key = value)
  )
  Process.env

let _ : unit Promise.t =
  let open Promise in
  let* contents = Fs.readdir "."
  and+ contents' = Fs.readdir "." in
  List.iter2 (fun x y -> assert (x = y)) contents contents';
  return ()
