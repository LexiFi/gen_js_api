module Selected = Ppxlib.Select_ast(Migrate_parsetree.Versions.OCaml_408)

let copy_attribute (a : Migrate_parsetree.Ast_408.Parsetree.attribute)
  : Ppxlib.Ast.attribute =
  let pat : Migrate_parsetree.Ast_408.Parsetree.pattern =
    Migrate_parsetree.Ast_408.Ast_helper.Pat.any ~attrs:[a] ()
  in
  let pat = Selected.Of_ocaml.copy_pattern pat in
  List.hd pat.ppat_attributes

let check_attributes_with_ppxlib = false
let check_locations_with_ppxlib = false

let () =
  if check_attributes_with_ppxlib
  then (
    Ppxlib.Driver.enable_checks ();
    Gen_js_api_ppx.check_attribute := false
  );
  if check_locations_with_ppxlib
  then (
    Ppxlib.Driver.enable_location_check ()
  );
  Gen_js_api_ppx.mark_as_handled_manually := (fun attribute ->
    let attribute = copy_attribute attribute in
    Ppxlib.Attribute.mark_as_handled_manually attribute);
  let mapper = Selected.Of_ocaml.copy_mapper Gen_js_api_ppx.mapper in
  let mapper_for_sig =
    Selected.Of_ocaml.copy_mapper
      (Gen_js_api_ppx.mark_attributes_as_used Gen_js_api_ppx.mapper)
  in
  Ppxlib.Driver.register_transformation
    "gen_js_api"
    ~impl:(fun str -> mapper.structure mapper str)
    ~intf:(fun sig_ ->
      mapper_for_sig.signature mapper_for_sig sig_)
