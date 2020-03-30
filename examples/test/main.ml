(* The gen_js_api is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2015 by LexiFi.                                              *)

(** Some ad hoc code to illustrate and test various aspects
    of gen_js_api *)

open Test_bindings

include
  ([%js] :
   sig
     val wrapper: (int -> int -> int) -> (int -> int -> int [@js.dummy])
         [@@js.global "wrapper"]

     val caller: (unit -> int) -> int
         [@@js.global "caller"]

     val caller_unit: (unit -> unit) -> unit
         [@@js.global "caller"]

     val test_variadic: ((int list [@js.variadic]) -> int) -> unit
     val test_variadic2: (string -> (int list [@js.variadic]) -> int) -> unit
   end)

module LocalBindings : sig
  type myType = { x : a; y : b [@js "Y"]}
  and a = int option
  and b = { s : string; i : int }

end = [%js]


let () =
  let s = [%js.of: int list] [10; 20; 30] in
  Printf.printf "%i\n%!" ([%js.to: int] (Ojs.array_get s 0));
  Printf.printf "%i\n%!" ([%js.to: int] (Ojs.array_get s 1));
  Printf.printf "%i\n%!" ([%js.to: int] (Ojs.array_get s 2))

let () =
  let sum xs = List.fold_left ( + ) 0 xs in
  test_variadic sum;
  test_variadic2 (fun msg xs -> Printf.printf "%s\n%!" msg; sum xs)

val myArray: int array
    [@@js]

val myArray2: Ojs.t
    [@@js.global "myArray"]

val alert_bool: bool -> unit
    [@@js.global "alert"]

val alert_float: float -> unit
    [@@js.global "alert"]


val test_opt_args: (?foo:int -> ?bar:int -> unit-> string) -> unit
  [@@js.global]

let doc = Window.document window

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
  Array.iter (Printf.printf "[%i]\n") myArray;

  Ojs.array_set myArray2 0 (Ojs.int_to_js 10);
  Ojs.array_set myArray2 1 (Ojs.array_to_js Ojs.int_to_js [| 100; 200; 300 |]);
(*  Ojs.array_set myArray2 1 ([%to_js: int array] [| 100; 200; 300 |]); *)

(*
  Printf.printf "%0.2f\n" 3.1415;
*)
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
  alert_bool true;
  alert_float 3.1415;
  let f =
    wrapper
      (fun x y ->
         Printf.printf "IN CALLBACK, x = %i, y = %i\n%!" x y;
         x + y
      )
  in
  Printf.printf "Result -> %i\n%!" (f 42 1);

  let uid = ref 0 in
  let f () =
    incr uid;
    Printf.printf "uid = %i\n%!" !uid;
    !uid
  in
  Printf.printf "Caller result -> %i, %i, %i\n%!" (caller f) (caller f) (caller f);
  caller_unit (fun () -> ignore (f ()));
  caller_unit (fun () -> ignore (f ()));
  caller_unit (fun () -> ignore (f ()));

  let alice = Person.create "Alice" Person.Foo.Foo in
  let bob = Person.create "Bob" Person.Foo.Bar in
  let charlie = Person.create "Charlie" (Person.Foo.OtherString "bla") in
  let eve = Person.create "Eve" (Person.Foo.OtherInt 2713) in

  Ojs.iter_properties (Person.cast alice) (Format.printf "%s\n%!");

  let alice_obj = PersonObj.create "Alice" Person.Foo.Foo in
  let bob_obj = PersonObj.of_person bob in
  let dave_obj = new PersonObj.person "Dave" Person.Foo.Bar [1; 2; 3] in

  let string_of_foo = function
    | Person.Foo.Foo -> "foo"
    | Person.Foo.Bar -> "bar"
    | Person.Foo.OtherInt n -> Printf.sprintf "other = %d" n
    | Person.Foo.OtherString s -> Printf.sprintf "other = %s" s
  in
  let string_of_name_foo name foo = Printf.sprintf "%s <%s>" name (string_of_foo foo) in
  let string_of_person x = string_of_name_foo (Person.name x) (Person.foo x) in
  let string_of_person_obj x = string_of_name_foo (x # name) (x # foo) in
  let hack_person x =
    let name, foo = Person.get x () in
    Printf.printf "before: %s <%s>\n" name (string_of_foo foo);
    Person.set x ("Dave", Person.Foo.OtherString "bar");
    let name, foo = Person.get x () in
    Printf.printf "after: %s <%s>\n" name (string_of_foo foo);
  in

  let body = Document.body doc in
  setTimeout (fun () -> Element.setAttribute body "bgcolor" "red") 2000;
  Element.appendChild body (Document.createTextNode doc "ABC");
  Element.appendChild body
    (div ~attrs:["style", "color: blue"] [ txt "!!!!"; elt "b" [txt "XXX"]]);

  Element.appendChild body
    (div (List.map (fun x -> txt (string_of_person x)) [alice; bob; charlie; eve]));
  hack_person eve;
  Element.appendChild body
    (div (List.map (fun x -> txt (string_of_person x)) [alice; bob; charlie; eve]));
  Element.appendChild body
    (div (List.map (fun x -> txt (string_of_person_obj x)) [alice_obj; bob_obj; dave_obj]));

  let s = (new Str.str "") # concat [Str.create "Hello"; Str.create ", "; Str.create "world"; Str.create "!"] in
  Console.log_string console (s # to_string);

  Console.log_string console (Date.to_string (Date.create ~year:2015 ~month:4 ~day:10 ()));

  let l = Document.getElementsByClassName doc "myClass" in
  Array.iter
    (fun e ->
       Printf.printf "- [%s]\n" (Element.innerHTML e); (* OK *)
       print_string (Printf.sprintf "+ [%s]\n" (Element.innerHTML e)); (* BAD *)

       Element.appendChild e (button "Click!" draw);
       Element.appendChild e (button "XXX" (fun () -> ()));
    )
    l;

  test_opt_args
    (fun ?(foo = 0) ?(bar = 0) () -> string_of_int foo ^ "/" ^ string_of_int bar);

  print_endline Person2.(to_json (mk ~children:[mk ~age:6 "Johnny"] ~age:42 "John Doe"))



(* Custom mapping between association lists and JS objects *)

module Dict : sig
  type 'a t = (string * 'a) list
  val t_to_js: ('a -> Ojs.t) -> 'a t -> Ojs.t
  val t_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a t
end = struct
  type 'a t = (string * 'a) list

  let t_to_js ml2js l =
    let o = Ojs.empty_obj () in
    List.iter (fun (k, v) -> Ojs.set o k (ml2js v)) l;
    o

  let t_of_js js2ml o =
    let l = ref [] in
    Ojs.iter_properties o
      (fun k -> l := (k, js2ml (Ojs.get o k)) :: !l);
    !l
end

val int_dict_to_json_string: int Dict.t -> string
  [@@js.global "JSON.stringify"]

val myDict: string Dict.t
  [@@js.global "myDict"]

val set_x: int -> unit
    [@@js.set "x"]

val get_x: unit -> int
    [@@js.get "x"]

let () =
  print_endline (int_dict_to_json_string ["hello", 1; "world", 2]);
  List.iter (fun (k, v) -> Printf.printf "%s -> %s\n%!" k v) myDict;
  set_x 42;
  print_endline (string_of_int (get_x ()))

module Sum = struct
include ([%js]: sig
           type t =
             | A
             | B of int
             | C of int * string
             | D of {age:int; name:string}
                   [@@js.sum]

           val t_of_js: Ojs.t -> t
           val t_to_js: t -> Ojs.t
         end)

let print = function
  | A -> print_endline "A"
  | B n -> print_endline (Format.sprintf "B %d" n)
  | C (n, s) -> print_endline (Format.sprintf "C (%d, %S)" n s)
  | D {age; name} -> print_endline (Format.sprintf "D {age = %d; name = %S}" age name)

val set_print_sum: (t -> unit) -> unit
    [@@js.set "print_sum"]

val test_sum: unit -> unit
    [@@js.global "test_sum"]

let () =
  set_print_sum print

let () = test_sum ()

let () =
  Console.log console ([%js.of:t] A);
  Console.log console ([%js.of:t] (B 42));
  Console.log console ([%js.of:t] (C (42, "foo")));
  Console.log console ([%js.of:t] (D {age=42; name="foo"}))
end

val test_flatten: ([`A | `B of int | `C of string | `D of int * string] [@js.enum]) -> unit
    [@@js.global "test_flatten"]

let () =
  test_flatten `A;
  test_flatten (`B 42);
  test_flatten (`C "hello");
  test_flatten (`D (42, "foo"))
