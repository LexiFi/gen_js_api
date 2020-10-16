open Node

let () = assert (Path.sep = Filename.dir_sep)

let () =
 Container.StringMap.iter
  (fun key value ->
    assert (Sys.getenv key = value)
  )
  Process.env
