module StringMap = struct
  include Map.Make(String)

  let t_to_js ml2js l =
    let o = Jsoo_runtime.Js.obj [||] in
    iter (fun k v -> Jsoo_runtime.Js.set o (Ojs.string_to_js k) (ml2js v)) l;
    o

  include
    [%js:
      val getOwnPropertyNames: Ojs.t -> string array [@@js.global "Object.getOwnPropertyNames"]
    ]

  let iter_properties obj f =
    Array.iter f (getOwnPropertyNames obj)

  let t_of_js js2ml o =
    let l = ref empty in
    iter_properties o
      (fun k -> l := add k (js2ml (Jsoo_runtime.Js.get o (Ojs.string_to_js k))) !l);
    !l
end
