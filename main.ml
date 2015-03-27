open Test_js

let doc = Window.document Window.__

let elt name ?(attrs = []) ?onclick subs =
  let e = Document.createElement doc name in
  List.iter (fun (k, v) -> Element.setAttribute e k v) attrs;
  List.iter (Element.appendChild e) subs;
  begin match onclick with
  | Some f -> Element.set_onclick e f
  | None -> ()
  end;
  e

let txt =
  Document.createTextNode doc

let button ?attrs s onclick =
  elt "button" ?attrs ~onclick [ txt s ]

let div = elt "div"

let () =
(*
  Printf.printf "%0.2f\n" 3.1415;
*)
  let doc = Window.document Window.__ in
(*
  Document.set_title doc "MyTitle";
  Document.set_title doc (Document.title doc ^ " :-)");
*)

(*  let main = Document.getElementById doc "main" in *)
(*  print_endline (Element.innerHTML main); *)
(*  alert (Element.innerHTML main); *)
(*  Element.set_innerHTML main "<b>Bla</b>blabla"; *)


  let draw () =
    let canvas_elt = Document.getElementById doc "canvas" in
    let canvas = Canvas.of_element canvas_elt in
    let ctx = Canvas.getContext_2d canvas in
    Canvas.RenderingContext2D.(begin
        set_fillStyle ctx "rgba(0,0,255,0.1)";
        fillRect ctx 30 30 50 50
      end);
    Element.set_onclick canvas_elt (fun () -> alert "XXX");
  in


  let body = Document.body doc in
  setTimeout (fun () -> Element.setAttribute body "bgcolor" "red") 2000;
  Element.appendChild body (Document.createTextNode doc "ABC");
  Element.appendChild body
    (div ~attrs:["style", "color: blue"] [ txt "!!!!"; elt "b" [txt "XXX"] ]);

  let l = Document.getElementsByClassName doc "myClass" in
  Array.iter
    (fun e ->
       Printf.printf "- [%s]\n" (Element.innerHTML e); (* OK *)
       print_string (Printf.sprintf "+ [%s]\n" (Element.innerHTML e)); (* BAD *)

       Element.appendChild e (button "Click!" draw);
       Element.appendChild e (button "XXX" (fun () -> ()));
    )
    l;
