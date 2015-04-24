let () =
  let s = new Stdlib.String.t "Hello world" in
  print_endline (s # to_string);
  for i = 0 to s # length - 1 do
    print_endline ((s # char_at i) # to_string)
  done;
  for i = 0 to s # length - 1 do
    print_endline (string_of_int (s # char_code_at i));
  done

let () =
  let s = Stdlib.String.from_char_code [66; 111; 110; 106; 111; 117; 114; 32; 108; 101; 32; 109; 111; 110; 100; 101] in
  print_endline (s # to_string)

let () =
  let s = new Stdlib.String.t "Hello world" in
  let s2 = new Stdlib.String.t "world" in
  print_endline (string_of_int (s # index_of s2 ()))

let () =
  let s = new Stdlib.String.t "Hello world" in
  let r = new Stdlib.RegExp.t "o" ~flags:"g" () in
  print_endline (r # to_string);
  let a = s # match_ r in
  for i = 0 to Array.length a - 1 do
    print_endline (a.(i) # to_string)
  done
