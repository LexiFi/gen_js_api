let () =
  Migrate_parsetree.Driver.register
    ~name:"gen_js_api"
    Migrate_parsetree.Versions.ocaml_408
    (fun _config _cookies -> Gen_js_api_ppx.mapper)
