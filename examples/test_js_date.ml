open Js_date

let at i =
  Printf.printf "Line %i: " i

let () =
  at __LINE__;
  print_endline (to_string (now()))

let () =
  at __LINE__;
  print_endline (to_string (from_string "October 13, 2014 11:13:00"))

let () =
  at __LINE__;
  print_endline (to_string (from_milliseconds 86400000.))

let () =
  at __LINE__;
  print_endline (to_string (create ~year:99 ~month:5 ~day:24 ~hours:11 ~minutes:33 ~seconds:30 ~ms:0 ()))

let () =
  at __LINE__;
  print_endline (to_string (create ~year:99 ~month:5 ~day:24 ()))

let () =
  at __LINE__;
  let d = now() in
  print_endline (string_of_int (get_UTC_date d))

let () =
  at __LINE__;
  let d = now() in
  print_endline (string_of_int (get_UTC_day d))

let () =
  at __LINE__;
  let d = now() in
  print_endline (string_of_int (get_UTC_full_year d))

let () =
  at __LINE__;
  let d = now() in
  print_endline (string_of_int (get_UTC_hours d))

let () =
  at __LINE__;
  let d = now() in
  print_endline (string_of_int (get_UTC_milliseconds d))

let () =
  at __LINE__;
  let d = now() in
  print_endline (string_of_int (get_UTC_minutes d))

let () =
  at __LINE__;
  let d = now() in
  print_endline (string_of_int (get_UTC_month d))

let () =
  at __LINE__;
  let d = now() in
  print_endline (string_of_int (get_UTC_seconds d))

let () =
  at __LINE__;
  let d = now() in
  set_UTC_date d 15;
  print_endline (to_UTC_string d)

let () =
  at __LINE__;
  let d = now() in
  set_UTC_full_year d 1992;
  print_endline (to_UTC_string d)

let () =
  at __LINE__;
  let d = now() in
  set_UTC_hours d 13;
  print_endline (to_UTC_string d)

let () =
  at __LINE__;
  let d = now() in
  set_UTC_milliseconds d 806;
  print_endline (to_UTC_string d)

let () =
  at __LINE__;
  let d = now() in
  set_UTC_minutes d 13;
  print_endline (to_UTC_string d)

let () =
  at __LINE__;
  let d = now() in
  set_UTC_month d 1;
  print_endline (to_UTC_string d)

let () =
  at __LINE__;
  let d = now() in
  set_UTC_seconds d 42;
  print_endline (to_UTC_string d)

let () =
  at __LINE__;
  let d = now() in
  print_endline (string_of_int (get_date d))

let () =
  at __LINE__;
  let d = now() in
  print_endline (string_of_int (get_day d))

let () =
  at __LINE__;
  let d = now() in
  print_endline (string_of_int (get_full_year d))

let () =
  at __LINE__;
  let d = now() in
  print_endline (string_of_int (get_hours d))

let () =
  at __LINE__;
  let d = now() in
  print_endline (string_of_int (get_milliseconds d))

let () =
  at __LINE__;
  let d = now() in
  print_endline (string_of_int (get_minutes d))

let () =
  at __LINE__;
  let d = now() in
  print_endline (string_of_int (get_month d))

let () =
  at __LINE__;
  let d = now() in
  print_endline (string_of_int (get_seconds d))

let () =
  at __LINE__;
  let d = now() in
  set_date d 15;
  print_endline (to_string d)

let () =
  at __LINE__;
  let d = now() in
  set_full_year d 1992;
  print_endline (to_string d)

let () =
  at __LINE__;
  let d = now() in
  set_hours d 13;
  print_endline (to_string d)

let () =
  at __LINE__;
  let d = now() in
  set_milliseconds d 806;
  print_endline (to_string d)

let () =
  at __LINE__;
  let d = now() in
  set_minutes d 13;
  print_endline (to_string d)

let () =
  at __LINE__;
  let d = now() in
  set_month d 1;
  print_endline (to_string d)

let () =
  at __LINE__;
  let d = now() in
  set_seconds d 42;
  print_endline (to_string d)

let () =
  at __LINE__;
  let d = now() in
  print_endline (string_of_float (get_time d))

let () =
  at __LINE__;
  let d = now() in
  set_time d 1332403882588.;
  print_endline (to_string d)

let () =
  at __LINE__;
  let d = now() in
  print_endline (string_of_int (get_timezone_offset d))

let () =
  at __LINE__;
  let d = now() in
  print_endline (to_locale_date_string d)

let () =
  at __LINE__;
  let d = now() in
  print_endline (to_locale_string d)

let () =
  at __LINE__;
  let d = now() in
  print_endline (to_locale_time_string d)

let () =
  at __LINE__;
  let d = now() in
  print_endline (to_date_string d)

let () =
  at __LINE__;
  let d = now() in
  print_endline (to_time_string d)

let () =
  at __LINE__;
  let d = now() in
  print_endline (to_UTC_string d)
