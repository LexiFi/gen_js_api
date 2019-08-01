module Element : sig
  type t

  val t_of_js: Ojs.t -> t

  val append_child: t -> t -> unit [@@js.call]

  val set_attribute: t -> string -> string -> unit

  val set_onclick: t -> (unit -> unit) -> unit
end = [%js]

module Window : sig
  type t

  val instance: t [@@js.global "window"]

  val set_onload: t -> (unit -> unit) -> unit
end = [%js]

module Document : sig
  type t

  val instance: t [@@js.global "document"]

  val create_element: t -> string -> Element.t

  val create_text_node: t -> string -> Element.t

  val body: t -> Element.t
end = [%js]

let element tag children =
  let elt = Document.create_element Document.instance tag in
  List.iter (Element.append_child elt) children;
  elt

let textnode s = Document.create_text_node Document.instance s

let td ?colspan child =
  let elt = element "td" [child] in
  begin match colspan with
  | None -> ()
  | Some n -> Element.set_attribute elt "colspan" (string_of_int n)
  end;
  elt

let tr = element "tr"
let table = element "table"
let center x = element "center" [x]

let button x f =
  let elt = element "button" [textnode x] in
  Element.set_attribute elt "type" "button";
  Element.set_onclick elt f;
  elt

module Engine = struct
  type op = Add | Sub | Mul | Div

  type state =
    {
      x: float;
      y: float;
      operator: op option;
      input: bool;
      equal: bool;
      comma: int;
    }

  let initial = { x = 0.; y = 0.; operator = None; input = false; equal = false; comma = 0 }

  let make_op op x y =
    match op with
    | Add -> x +. y
    | Sub -> x -. y
    | Mul -> x *. y
    | Div -> x /. y

  let of_digit d = float_of_int d
  let add_digit x comma d =
    if comma = 0 then 10. *. x +. float_of_int d, comma
    else x +. float_of_int d /. (10. ** (float_of_int comma)), comma + 1

  let input_digit ({x; y; operator = _; input; equal; comma} as state) d =
    let y = if equal then y else x in
    let x, comma =
      if input then add_digit x comma d
      else of_digit d, 0
    in
    {state with x; y; comma; input = true}

  let apply_comma ({input; comma; _} as state) =
    if comma = 0 then
      if input then {state with comma = 1}
      else {(input_digit state 0) with comma = 1}
    else state

  let apply_equal ({x; y; operator; input; equal; comma = _} as state) =
    match operator with
    | None -> {state with y = x; input = false; equal = true}
    | Some o ->
        if input && not equal then {state with x = make_op o y x; y = x; input = false; equal = true}
        else {state with x = make_op o x y; equal = true}

  let apply_op ({input; equal; _} as state) op =
    if input && not equal then {(apply_equal state) with operator = Some op; equal = false}
    else {state with operator = Some op; equal= false; input = false}

  let print_op ppf = function
    | None -> Printf.fprintf ppf " "
    | Some Add -> Printf.fprintf ppf "+"
    | Some Sub -> Printf.fprintf ppf "-"
    | Some Mul -> Printf.fprintf ppf "*"
    | Some Div -> Printf.fprintf ppf "/"

  let print ppf {x; y; operator; input; equal; comma} =
    Printf.fprintf ppf "x = %g, y = %g, op = %a, input = %b, equal = %b, comma = %d" x y print_op operator input equal comma
end


let widget () =
  let open Engine in
  let state = ref initial in
  let res, set_value =
    let elt = element "input" [] in
    Element.set_attribute elt "type" "text";
    Element.set_attribute elt "readonly" "";
    let set_value v = Element.set_attribute elt "value" (string_of_float v) in
    elt, set_value
  in
  let update st =
    Printf.printf "%a\n" print st;
    state := st;
    set_value !state.x
  in
  let reset() = update initial in
  reset();
  let binop op () = update (apply_op !state op) in
  let equal () = update (apply_equal !state) in
  let comma () = update (apply_comma !state) in
  let figure digit =
    let f () = update (input_digit !state digit) in
    button (string_of_int digit) f
  in
  let c l = td l in
  let nothing () = element "div" [] in
  table [tr [td ~colspan:4 res];
         tr (List.map c [nothing(); button "C" reset; nothing(); button "/" (binop Div)]);
         tr (List.map c [figure 7; figure 8; figure 9; button "*" (binop Mul)]);
         tr (List.map c [figure 4; figure 5; figure 6; button "-" (binop Sub)]);
         tr (List.map c [figure 1; figure 2; figure 3; button "+" (binop Add)]);
         tr (List.map c [nothing(); figure 0; button "." comma; button "=" equal])]

let go () =
  Element.append_child (Document.body Document.instance) (center (widget()))

let () =
  Window.set_onload Window.instance go
