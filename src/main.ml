open Ppxlib
open Util

(* Declaration of extension [%HOLE] *)
let hole =
  Extension.declare "HOLE" Extension.Context.expression
    Ast_pattern.(pstr nil)
    (fun ~loc ~path:_ -> mkhole ~loc)

(* open%fillup M, open module as instances *)
let open_instance_toplevel =
  Extension.declare "fillup" Extension.Context.structure_item
    Ast_pattern.(pstr @@ pstr_open __ ^:: nil)
    (fun ~loc ~path:_ open_module ->
      let mod_exp = open_module.popen_expr in
      let dummy_name = mk_dummy_module () in
      {
        pstr_desc =
          Pstr_module
            {
              pmb_name = mkloc ~loc @@ Some dummy_name;
              pmb_expr = mod_exp;
              pmb_attributes = [];
              pmb_loc = loc;
            };
        pstr_loc = loc;
      })

(* let open%fillup M in e, open module locally as instances *)
let open_instance_local =
  Extension.declare "fillup" Extension.Context.expression
    Ast_pattern.(pstr @@ pstr_eval (pexp_open __ __) nil ^:: nil)
    (fun ~loc ~path:_ md expr ->
      let md_exp = md.popen_expr in
      let dummy_name = mk_dummy_module () in
      {
        pexp_desc = Pexp_letmodule (mkloc ~loc @@ Some dummy_name, md_exp, expr);
        pexp_loc = loc;
        pexp_loc_stack = [];
        pexp_attributes = [];
      })

let transform (str : Parsetree.structure) =
  Selected_ast.Of_ocaml.copy_structure
  @@ Fillup.loop_typer_untyper
  @@ Selected_ast.To_ocaml.copy_structure
  @@ expr_mapper Fillup.replace_hashhash_with_holes str

let () =
  Driver.register_transformation
    ~extensions:[ hole; open_instance_toplevel; open_instance_local ]
    ~instrument:(Driver.Instrument.make ~position:After transform)
    "ppx_fillup"
