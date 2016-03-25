(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

(** A toy application built with jQuery *)

open Jquery

val alert: string -> unit
  [@@js.global]

let ( !! ) = Jquery.selector

let block s ?text ?(classes = []) ?(ons = []) ?(css = []) ?(props = []) children =
  let element = Jquery.selector (Printf.sprintf "<%s>" s) in
  begin match text with
  | None -> ()
  | Some text -> Jquery.set_text element text
  end;
  List.iter (fun c -> Jquery.add_class element c) classes;
  List.iter (fun (key, value) -> Jquery.set_css_value element key value) css;
  List.iter (fun (key, value) -> Jquery.set_prop element key value) props;
  List.iter (fun (event, f) -> Jquery.on element event f) ons;
  begin match children with
    | [] -> ()
    | _ :: _-> Jquery.append element children
  end;
  element

let ajax_test () =
  let open Ajax in
  let complete h = function
    | "success" ->
        let pre = block "pre" ~text:(response_text h) [] in
        hide pre;
        append !!"body" [pre];
        fade_in pre ~duration:2000
          ~finished:(fun () ->
              fade_out pre ~finished:(fun () -> detach pre) ()
            )
          ()
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

  append main [elts; !! "<b>XXX</b>"];

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

  let div = block "div" [] in
  let input = block "input" [] in
  on input "input" (fun _ -> set_text div (get_val input));
  append main [input; div];

  let btn =
    block "button" ~text:"SHOW SOURCE CODE" []
      ~ons:["click", (fun _ -> ajax_test ())]
  in
  append main [btn]

let () =
  ready on_ready
