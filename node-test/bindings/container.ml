module StringMap = struct
  include Map.Make(String)

  let t_to_js ml2js l =
    let o = Ojs.empty_obj () in
    iter (fun k v -> Ojs.set o k (ml2js v)) l;
    o

  let t_of_js js2ml o =
    let l = ref empty in
    Ojs.iter_properties o
      (fun k -> l := add k (js2ml (Ojs.get o k)) !l);
    !l
end