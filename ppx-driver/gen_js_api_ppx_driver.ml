let mapper = Gen_js_api_ppx.mapper

module Selected = Ppxlib.Select_ast(Migrate_parsetree.Versions.OCaml_408)

let copy_attribute (a : Migrate_parsetree.Ast_408.Parsetree.attribute)
  : Ppxlib.Ast.attribute =
  let pat : Migrate_parsetree.Ast_408.Parsetree.pattern =
    Migrate_parsetree.Ast_408.Ast_helper.Pat.any ~attrs:[a] ()
  in
  let pat = Selected.Of_ocaml.copy_pattern pat in
  List.hd pat.ppat_attributes

let () =
  (* Ppxlib's checks can be enabled with the following lines *)
  (* {[
        Ppxlib.Driver.enable_checks ();
        Gen_js_api_ppx.check_attribute := false;
     ]}
   *)
  Gen_js_api_ppx.mark_as_handled_manually := (fun attribute ->
    let attribute = copy_attribute attribute in
    Ppxlib.Attribute.mark_as_handled_manually attribute);
  let mapper = Selected.Of_ocaml.copy_mapper mapper in
  Ppxlib.Driver.register_transformation
    "gen_js_api"
    ~impl:(fun str -> mapper.structure mapper str)
    ~intf:(fun sig_ -> mapper.signature mapper sig_)
