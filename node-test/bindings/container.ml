module StringMap = struct
  include Map.Make(String)

  let t_to_js (type s) (s_to_js : s -> Ojs.t) m =
    [%js.of: (string * s) list] (bindings m)

  let t_of_js (type s) (s_of_js : Ojs.t -> s) o =
    let l = [%js.to: (string * s) list] o in
    List.fold_left (fun acc (k, o) -> add k o acc) empty l

end
