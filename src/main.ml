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
    (fun ~loc ~path:_ md ->
      let md_exp = md.popen_expr in
      let dummy_name = mk_dummy_md_name () in
      {
        pstr_desc =
          Pstr_module
            {
              pmb_name = mkloc ~loc @@ Some dummy_name;
              pmb_expr = md_exp;
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
      let dummy_name = mk_dummy_md_name () in
      {
        pexp_desc = Pexp_letmodule (mkloc ~loc @@ Some dummy_name, md_exp, expr);
        pexp_loc = loc;
        pexp_loc_stack = [];
        pexp_attributes = [];
      })

(* [%%open_inst M(a,b,c)], open expressions (a,b,c) in module M as instances *)
let open_instance =
  let open Ppxlib.Ast_helper in
  Extension.declare "open_inst" Extension.Context.structure_item
    Ast_pattern.(pstr @@ pstr_eval (pexp_construct __ __) nil ^:: nil)
    (fun ~loc ~path:_ lid expop ->
      (* let md_lid_loc = mkloc ~loc lid in *)
      let expop_of_str = function
        | Some { pexp_desc = Pexp_tuple l; _ } ->
            let rec loop acc = function
              | { pexp_desc = Pexp_ident lid_loc; _ } :: rest ->
                  rest
                  |> loop
                     @@ (Str.eval
                        @@ Exp.ident
                        @@ mkloc ~loc
                        @@ Ldot (lid, Longident.name lid_loc.txt))
                        :: acc
              | [] -> acc
              | _ -> assert false
            in
            loop [] l
        | Some { pexp_desc = Pexp_ident lid_loc; _ } ->
            [
              Str.eval
              @@ Exp.ident
              @@ mkloc ~loc
              @@ Ldot (lid, Longident.name lid_loc.txt);
            ]
        | None -> []
        | Some _ ->
            Location.raise_errorf ~loc "(ppx_fillup) Invalid syntax: %s %a"
              (Longident.name lid) Pprintast.expression (Option.get expop)
      in
      let dummy_md_name = mkloc ~loc @@ Some (mk_dummy_md_name ()) in
      Str.module_ @@ Mb.mk dummy_md_name @@ Mod.structure (expop_of_str expop))

let transform (str : Parsetree.structure) =
  Fillup.replace_hashhash str
  |> Selected_ast.To_ocaml.copy_structure
  |> Fillup.typer_untyper
  |> Selected_ast.Of_ocaml.copy_structure

let () =
  Driver.register_transformation
    ~extensions:
      [ hole; open_instance_toplevel; open_instance_local; open_instance ]
    ~instrument:(Driver.Instrument.make ~position:After transform)
    "ppx_fillup"
