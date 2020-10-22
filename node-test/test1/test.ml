open Node

let () = assert (Path.sep = Filename.dir_sep)

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