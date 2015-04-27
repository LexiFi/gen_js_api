open Js_str

let at i =
  Printf.printf "Line %i: " i

let print_array f a =
  Array.iteri
    (fun i x ->
       Printf.printf "%i:%s " i (f x);
    ) a;
  print_newline()

let () =
  at __LINE__;
  let str = of_string "HELLO WORLD" in
  let res = char_at str 0 in
  print_endline (to_string res)

let () =
  at __LINE__;
  let str = of_string "HELLO WORLD" in
  let res = char_at str (length str - 1) in
  print_endline (to_string res)

let () =
  at __LINE__;
  let str = of_string "HELLO WORLD" in
  let n = char_code_at str 0 in
  print_endline (string_of_int n)

let () =
  at __LINE__;
  let str1 = of_string "Hello " in
  let str2 = of_string "world!" in
  let res = concat str1 [str2] in
  print_endline (to_string res)

let () =
  at __LINE__;
  let res = from_char_code [65] in
  print_endline (to_string res)

let () =
  at __LINE__;
  let str = of_string "Hello world, welcome to the universe." in
  let n = index_of str (of_string "welcome") () in
  print_endline (string_of_int n)

let () =
  at __LINE__;
  let str = of_string "Hello planet earth, you are a great planet." in
  let n = last_index_of str (of_string "planet") () in
  print_endline (string_of_int n)

let () =
  at __LINE__;
  let str1 = of_string "ab" in
  let str2 = of_string "cd" in
  let n = locale_compare str1 str2 in
  print_endline (string_of_int n)

let () =
  at __LINE__;
  let str = of_string "The rain in SPAIN stays mainly in the plain" in
  let res = match_ str (regexp (of_string "ain") ~global:() ()) in
  match res with
  | None -> print_endline "no match"
  | Some s -> print_array to_string s

let () =
  at __LINE__;
  let str = of_string "Visit Microsoft!" in
  let res = replace str (regexp (of_string "Microsoft") ()) (of_string "W3Schools") in
  print_endline (to_string res)

let () =
  at __LINE__;
  let str = of_string "Visit W3Schools!" in
  let n = search str (regexp (of_string "W3Schools") ()) in
  print_endline (string_of_int n)

let () =
  at __LINE__;
  let str = of_string "Hello world!" in
  let res = slice str ~start:1 ~end_:5 () in
  print_endline (to_string res)

let () =
  at __LINE__;
  let str = of_string "How are you doing today?" in
  let res = split str ~separator:(of_string " ") () in
  print_array to_string res

let () =
  at __LINE__;
  let str = of_string "Hello world!" in
  let res = substr str ~start:1 ~length:4 () in
  print_endline (to_string res)

let () =
  at __LINE__;
  let str = of_string "Hello world!" in
  let res = substring str ~start:1 ~end_:4 () in
  print_endline (to_string res)

let () =
  at __LINE__;
  let str = of_string "Hello World!" in
  let res = to_locale_lower_case str in
  print_endline (to_string res)

let () =
  at __LINE__;
  let str = of_string "Hello World!" in
  let res = to_locale_upper_case str in
  print_endline (to_string res)

let () =
  at __LINE__;
  let str = of_string "Hello World!" in
  let res = to_lower_case str in
  print_endline (to_string res)

let () =
  at __LINE__;
  let str = of_string "Hello World!" in
  let res = to_upper_case str in
  print_endline (to_string res)

let () =
  at __LINE__;
  let str = of_string "       Hello World!        " in
  let res = trim str in
  print_endline (to_string res)


let () =
  at __LINE__;
  let str = of_string "Hello world!" in
  let patt = regexp (of_string {|(.*) |}) ~global:() () in
  let res = exec patt str in
  match res with
  | None -> print_endline "no match"
  | Some s -> print_array to_string s

let () =
  at __LINE__;
  let str = of_string "Hello world!" in
  let patt = regexp (of_string "W3Schools") ~global:() () in
  let res = exec patt str in
  match res with
  | None -> print_endline "no match"
  | Some s -> print_array to_string s

let () =
  at __LINE__;
  let str = of_string "Hello world!" in
  let patt = regexp (of_string "Hello") ~global:() () in
  let res = test patt str in
  print_endline (string_of_bool res)

let () =
  at __LINE__;
  let str = of_string "Hello world!" in
  let patt = regexp (of_string "W3Schools") ~global:() () in
  let res = test patt str in
  print_endline (string_of_bool res)

let () =
  at __LINE__;
  let patt = regexp (of_string "W3S") ~global:() () in
  print_endline (string_of_bool (global patt))

let () =
  at __LINE__;
  let patt = regexp (of_string "W3S") ~ignore_case:() () in
  print_endline (string_of_bool (ignore_case patt))

let () =
  at __LINE__;
  let patt = regexp (of_string "W3S") ~global:() ~ignore_case:() () in
  print_endline (string_of_bool (multiline patt))

let () =
  at __LINE__;
  let patt = regexp (of_string "W3S") ~global:() () in
  print_endline ("The text of the RegExp is: " ^ source patt)

let () =
  at __LINE__;
  let str = of_string "The rain in Spain stays mainly in the plain" in
  let patt = regexp (of_string "ain") ~global:() () in
  while test patt str do
    print_endline ("'ain' found. Index now at: " ^ string_of_int (last_index patt))
  done
