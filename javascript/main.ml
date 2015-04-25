open Stdlib

let print_array f a =
  Array.iteri (fun i x -> if i > 0 then print_string ", "; print_string (f x)) a;
  print_newline()

let () =
  let str = new String.t "HELLO WORLD" in
  let res = str # char_at 0 in
  print_endline res # to_string

let () =
  let str = new String.t "HELLO WORLD" in
  let n = str # char_code_at 0 in
  print_endline (string_of_int n)

let () =
  let str1 = new String.t "Hello " in
  let str2 = new String.t "world!" in
  let res = str1 # concat [str2] in
  print_endline (res # to_string)

let () =
  let res = String.from_char_code [65] in
  print_endline (res # to_string)

let () =
  let str = new String.t "Hello world, welcome to the universe." in
  let n = str # index_of (new String.t "welcome") () in
  print_endline (string_of_int n)

let () =
  let str = new String.t "Hello planet earth, you are a great planet." in
  let n = str # last_index_of (new String.t "planet") () in
  print_endline (string_of_int n)

let () =
  let str1 = new String.t "ab" in
  let str2 = new String.t "cd" in
  let n = str1 # locale_compare str2 in
  print_endline (string_of_int n)

let () =
  let str = new String.t "The rain in SPAIN stays mainly in the plain" in
  let res = str # match_ (new RegExp.t "ain" ~flags:"g" ()) in
  print_array (fun s -> s # to_string) res

let () =
  let str = new String.t "Visit Microsoft!" in
  let res = str # replace (new RegExp.t "Microsoft" ()) (new String.t "W3Schools") in
  print_endline (res # to_string)

let () =
  let str = new String.t "Visit W3Schools!" in
  let n = str # search (new RegExp.t "W3Schools" ()) in
  print_endline (string_of_int n)

let () =
  let str = new String.t "Hello world!" in
  let res = str # slice ~start:1 ~end_:5 () in
  print_endline (res # to_string)

let () =
  let str = new String.t "How are you doing today?" in
  let res = str # split ~separator:(new String.t " ") () in
  print_array (fun s -> s # to_string) res

let () =
  let str = new String.t "Hello world!" in
  let res = str # substr ~start:1 ~length:4 () in
  print_endline (res # to_string)

let () =
  let str = new String.t "Hello world!" in
  let res = str # substring ~start:1 ~end_:4 () in
  print_endline (res # to_string)

let () =
  let str = new String.t "Hello World!" in
  let res = str # to_locale_lower_case in
  print_endline (res # to_string)

let () =
  let str = new String.t "Hello World!" in
  let res = str # to_locale_upper_case in
  print_endline (res # to_string)

let () =
  let str = new String.t "Hello World!" in
  let res = str # to_lower_case in
  print_endline (res # to_string)

let () =
  let str = new String.t "Hello World!" in
  let res = str # to_upper_case in
  print_endline (res # to_string)

let () =
  let str = new String.t "       Hello World!        " in
  let res = str # trim in
  print_endline (res # to_string)

let () =
  let str = new String.t "Hello World!" in
  print_endline (str # value_of)

let () =
  let str = new String.t "Hello world!" in
  let patt = new RegExp.t "Hello" ~flags:"g" () in
  let res = patt # exec str in
  match res with
  | None -> print_endline "null"
  | Some s -> print_endline (s # to_string)

let () =
  let str = new String.t "Hello world!" in
  let patt = new RegExp.t "W3Schools" ~flags:"g" () in
  let res = patt # exec str in
  match res with
  | None -> print_endline "null"
  | Some s -> print_endline (s # to_string)

let () =
  let str = new String.t "Hello world!" in
  let patt = new RegExp.t "Hello" ~flags:"g" () in
  let res = patt # test str in
  print_endline (string_of_bool res)

let () =
  let str = new String.t "Hello world!" in
  let patt = new RegExp.t "W3Schools" ~flags:"g" () in
  let res = patt # test str in
  print_endline (string_of_bool res)

let () =
  let patt = new RegExp.t "W3S" ~flags:"g" () in
  print_endline (string_of_bool (patt # global))

let () =
  let patt = new RegExp.t "W3S" ~flags:"i" () in
  print_endline (string_of_bool (patt # ignore_case))

let () =
  let patt = new RegExp.t "W3S" ~flags:"gi" () in
  print_endline (string_of_bool (patt # multiline))

let () =
  let patt = new RegExp.t "W3S" ~flags:"g" () in
  print_endline ("The text of the RegExp is: " ^ patt # source)

let () =
  let str = new String.t "The rain in Spain stays mainly in the plain" in
  let patt = new RegExp.t "ain" ~flags:"g" () in
  while patt # test str do
    print_endline ("'ain' found. Index now at: " ^ string_of_int patt # last_index)
  done
