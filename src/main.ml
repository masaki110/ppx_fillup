open Ppxlib
open Util

(* Declaration of extension [%HOLE] *)
(* let hole =
   Extension.declare "HOLE" Extension.Context.expression
     Ast_pattern.(pstr nil)
     (fun ~loc ~path:_ -> mkhole' ~loc ()) *)

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

(* let declare_placeholder =
     Extension.declare "declare_placeholder" Extension.Context.structure_item
       Ast_pattern.(pstr @@ pstr_eval (pexp_constant __) nil ^:: nil)
       (fun ~loc ~path:_ (Pconst_string (str, _, Some ":")) ->
         Ast_helper.(
           let attrs = [] in
           let pat = Pat.var ~loc ~attrs (mknoloc expr) in
           let expr = Exp. ~loc ~ attrs ()
           Str.value ~loc Nonrecursive
             [ Vb.mk ~loc ~attrs pat expr ])) *)

(* [%%open_inst M(a,b,c)], open expressions (a,b,c) in module M as instances *)
(* let open_instance =
   Extension.declare "open_inst" Extension.Context.structure_item
     Ast_pattern.(pstr @@ pstr_eval (pexp_construct __ __) nil ^:: nil)
     (fun ~loc ~path:_ lid expop ->
       let open Ppxlib.Ast_helper in
       let open Longident in
       let err ~loc =
         Location.raise_errorf ~loc "(ppx_fillup) Invalid syntax: %a"
           Pprintast.expression
           (Ast_helper.Exp.construct (mkloc ~loc lid) expop)
       in
       let str_of_construct ~loc lid = function
         | Some { pexp_desc = Pexp_tuple l; _ } ->
             let rec loop acc = function
               | { pexp_desc = Pexp_ident lid_loc; _ } :: rest ->
                   rest
                   |> loop
                        ((Str.eval
                         @@ Exp.ident
                         @@ mkloc ~loc (Ldot (lid, name lid_loc.txt)))
                        :: acc)
               | [] -> acc
               | _ -> err ~loc
             in
             loop [] l
         | Some { pexp_desc = Pexp_ident lid'; _ } ->
             [ Str.eval @@ Exp.ident @@ mkloc ~loc (Ldot (lid, name lid'.txt)) ]
         | _ -> err ~loc
       in
       let dummy_md_name = mkloc ~loc @@ Some (mk_dummy_md_name ()) in
       let stri =
         Str.module_ ~loc
         @@ Mb.mk ~loc dummy_md_name
         @@ Mod.structure ~loc (str_of_construct ~loc lid expop)
       in
       stri) *)

let transform (str : Parsetree.structure) =
  if
    Ocaml_common.Ast_mapper.tool_name () = "ocamldoc"
    || Ocaml_common.Ast_mapper.tool_name () = "ocamldep"
  then Fillup.preprocess str
  else
    Fillup.preprocess str
    |> Selected_ast.To_ocaml.copy_structure
    (* |> Fillup.alert_mapper *)
    |> Fillup.fillup
    |> Selected_ast.Of_ocaml.copy_structure

let () =
  Driver.register_transformation
    ~extensions:[ open_instance_toplevel; open_instance_local ]
    ~instrument:(Driver.Instrument.make transform ~position:After)
    "ppx_fillup"
