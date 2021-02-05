module From_ppx = Migrate_parsetree.Versions.OCaml_411
module Selected =  Ppxlib.Select_ast(From_ppx)

module Of_ppxlib = struct
  include Selected.To_ocaml
  let copy_rec_flag (rec_flag : Ppxlib.Asttypes.rec_flag) : From_ppx.Ast.Asttypes.rec_flag =
    match rec_flag with
    | Nonrecursive -> From_ppx.Ast.Asttypes.Nonrecursive
    | Recursive -> Recursive
end

module To_ppxlib = struct
  include Selected.Of_ocaml

  let copy_module_expr (m : From_ppx.Ast.Parsetree.module_expr) : Ppxlib.Parsetree.module_expr =
    match
      copy_structure
        [ From_ppx.Ast.Ast_helper.(Str.module_ (Mb.mk ({txt= Some "FAKE";loc=Location.none}) m))]
    with
    | [{pstr_desc=Pstr_module {pmb_expr;_}; _}] -> pmb_expr
    | _ -> assert false

  let copy_attribute (a : From_ppx.Ast.Parsetree.attribute)
    : Ppxlib.Ast.attribute =
    let pat : Migrate_parsetree.Ast_411.Parsetree.pattern =
      Migrate_parsetree.Ast_411.Ast_helper.Pat.any ~attrs:[a] ()
    in
    let pat = copy_pattern pat in
    List.hd pat.ppat_attributes
end


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
      let attribute = To_ppxlib.copy_attribute attribute in
      Ppxlib.Attribute.mark_as_handled_manually attribute);
  let mapper_for_sig =
    To_ppxlib.copy_mapper
      (Gen_js_api_ppx.mark_attributes_as_used Gen_js_api_ppx.mapper)
  in
  let mapper_for_str =
    To_ppxlib.copy_mapper
      (Gen_js_api_ppx.mark_attributes_as_used From_ppx.Ast.Ast_mapper.default_mapper)
  in
  let module_expr_ext =
    let rewriter ~loc ~path:_ si =
      si
      |> Of_ppxlib.copy_signature
      |> Gen_js_api_ppx.module_expr_rewriter ~loc ~attrs:[]
      |> To_ppxlib.copy_module_expr
    in
    Ppxlib.Extension.declare "js"
      Ppxlib.Extension.Context.Module_expr
      Ppxlib.(Ast_pattern.psig Ast_pattern.__)
      rewriter
    |> Ppxlib.Context_free.Rule.extension
  in
  let ext_to =
    let rewriter ~loc ~path:_ core_type =
      core_type
      |> Of_ppxlib.copy_core_type
      |> Gen_js_api_ppx.js_to_rewriter ~loc
      |> To_ppxlib.copy_expression
    in
    Ppxlib.Extension.declare "js.to"
      Ppxlib.Extension.Context.Expression
      Ppxlib.(Ast_pattern.ptyp Ast_pattern.__)
      rewriter
    |> Ppxlib.Context_free.Rule.extension
  in
  let ext_of =
    let rewriter ~loc ~path:_ core_type =
      core_type
      |> Of_ppxlib.copy_core_type
      |> Gen_js_api_ppx.js_of_rewriter ~loc
      |> To_ppxlib.copy_expression
    in
    Ppxlib.Extension.declare "js.of"
      Ppxlib.Extension.Context.Expression
      Ppxlib.(Ast_pattern.ptyp Ast_pattern.__)
      rewriter
    |> Ppxlib.Context_free.Rule.extension
  in
  Ppxlib.Driver.register_transformation
    "gen_js_api"
    ~rules:[module_expr_ext; ext_of; ext_to]
    ~impl:(fun str_ ->
        mapper_for_str.structure mapper_for_str str_)
    ~intf:(fun sig_ ->
        mapper_for_sig.signature mapper_for_sig sig_)

let deriver =
  let generate_intf ~ctxt (_, tdl) =
      tdl
      |> List.map (Of_ppxlib.copy_type_declaration)
      |> Gen_js_api_ppx.type_decl_sig_rewriter
        ~loc:(Ppxlib.Expansion_context.Deriver.derived_item_loc ctxt)
      |> To_ppxlib.copy_signature
  in
  let generate_impl ~ctxt (rec_flag, tdl) =
    tdl
    |> List.map (Of_ppxlib.copy_type_declaration)
    |> Gen_js_api_ppx.type_decl_rewriter
      ~loc:(Ppxlib.Expansion_context.Deriver.derived_item_loc ctxt)
      (Of_ppxlib.copy_rec_flag rec_flag)
    |> To_ppxlib.copy_structure
  in
  let open Ppxlib in
  let str_type_decl = Deriving.Generator.V2.make_noarg generate_impl in
  let sig_type_decl = Deriving.Generator.V2.make_noarg generate_intf in
  Deriving.add ~str_type_decl ~sig_type_decl "js"
