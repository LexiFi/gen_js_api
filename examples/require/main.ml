
let test_show_int () =
  Format.printf "a.show_int: %!";
  Test.A.show_int 42

let get_prop () =
  Format.printf "a.get_prop: %s@." (Test.A.prop ())

let set_prop () =
  Format.printf "a.set_prop; ";
  Test.A.set_prop "Hello sweet world!";
  get_prop ()

let add () =
  Format.printf "a.add 20 22: %d@." (Test.A.add 20 22)

let test_b () =
  let b = Test.B.new_b () in
  Format.printf "b.incr: %d; " (b#incr ());
  Format.printf "b.decr: %d; " (b#decr ());
  Format.printf "b.incr: %d; " (b#incr ());
  Format.printf "b.incr: %d; " (b#incr ());
  Format.printf "b.incr: %d;@." (b#incr ())

let test_c () =
  let c = Lazy.force Test.module_c in
  Format.printf "c.incr(41): %d@." (Test.C.incr c 41);
  Format.printf "c.show_string: %!";
  Test.C.show_string c "This is a C!";
  Format.printf "c.prop: %d@." (Test.C.prop c);
  Format.printf "c.set_prop;";
  Test.C.set_prop c 1337;
  Format.printf "c.prop: %d@." (Test.C.prop c)

let functions =
  [ test_show_int; get_prop; set_prop; add;
    test_b; test_c ]

let () = List.iter (fun f -> f ()) functions
