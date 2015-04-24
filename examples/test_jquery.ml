open Jquery

val alert: string -> unit
  [@@js.global]

let ( !! ) = Jquery.selector

let ajax_test () =
  let open Ajax in
  let complete h = function
    | "success" ->
        let pre = !!"<pre>" in
        set_text pre (response_text h);
        append !!"body" pre
    | status -> alert (Printf.sprintf "status = %s" status)
  in
  run (settings ~meth:`GET ~url:"test_jquery.ml" ~data_type:"text" ~complete ())

let on_ready () =
  let main = !!"#main" in
  print_endline (text main);
  set_text main "Hello world!";
  append_html main "<b>in bold</b>";

  let elts = !!".tofill" in
  update_text elts (Printf.sprintf "[%i:%s]");

  append main elts;
  append main (!! "<b>XXX</b>");

  let on_click evt =
    let open Event in
    append_html main
      (Printf.sprintf "<br/>x=%f,y=%f,type=%s"
         (page_x evt)
         (page_y evt)
         (type_ evt)
      )
  in
  on main "click" on_click;

  let div = !! "<div>" in
  let input = !! "<input>" in
  append main input;
  append main div;

  on input "input" (fun _ -> set_text div (get_val input));

  let btn = !! "<button>SHOW SOURCE CODE</button>" in
  on btn "click" (fun _ -> ajax_test ());
  append main btn

let () =
  ready on_ready
