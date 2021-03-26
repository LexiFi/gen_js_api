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
  let mapper_for_sig =
    Gen_js_api_ppx.mark_attributes_as_used
  in
  let mapper_for_str =
    Gen_js_api_ppx.mark_attributes_as_used
  in
  let module_expr_ext =
    let rewriter ~loc ~path:_ si =
      Gen_js_api_ppx.module_expr_rewriter ~loc ~attrs:[] si
    in
    Ppxlib.Extension.declare "js"
      Ppxlib.Extension.Context.Module_expr
      Ppxlib.(Ast_pattern.psig Ast_pattern.__)
      rewriter
    |> Ppxlib.Context_free.Rule.extension
  in
  let ext_to =
    let rewriter ~loc ~path:_ core_type =
      Gen_js_api_ppx.js_to_rewriter ~loc core_type
    in
    Ppxlib.Extension.declare "js.to"
      Ppxlib.Extension.Context.Expression
      Ppxlib.(Ast_pattern.ptyp Ast_pattern.__)
      rewriter
    |> Ppxlib.Context_free.Rule.extension
  in
  let ext_of =
    let rewriter ~loc ~path:_ core_type =
      Gen_js_api_ppx.js_of_rewriter ~loc core_type
    in
    Ppxlib.Extension.declare "js.of"
      Ppxlib.Extension.Context.Expression
      Ppxlib.(Ast_pattern.ptyp Ast_pattern.__)
      rewriter
    |> Ppxlib.Context_free.Rule.extension
  in
  let attr_typ =
    let rewriter ~ctxt (rec_flag : Ppxlib.Asttypes.rec_flag) tdl _ =
      Gen_js_api_ppx.type_decl_rewriter
        ~loc:(Ppxlib.Expansion_context.Deriver.derived_item_loc ctxt)
        rec_flag tdl
    in
    Ppxlib.Context_free.Rule.attr_str_type_decl
      (Ppxlib.Attribute.declare "js"
         Ppxlib.Attribute.Context.type_declaration
         Ppxlib.(Ast_pattern.pstr Ast_pattern.nil) ())
      rewriter
  in
  Ppxlib.Driver.register_transformation
    "gen_js_api"
    ~rules:[module_expr_ext; ext_of; ext_to; attr_typ ]
    ~impl:(mapper_for_str # structure)
    ~intf:(mapper_for_sig # signature)
