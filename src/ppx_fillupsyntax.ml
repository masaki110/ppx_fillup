open Ppx_fillupapi
open Ppxlib

(* Declaration of extension [%H cls] *)
(* let hole =
  Context_free.Rule.extension
  @@ Extension.declare
       "H"
       Extension.Context.expression
       Ast_pattern.(pstr @@ __ ^:: nil)
       (fun ~loc ~path:_ stri ->
         let idopt =
           match stri.pstr_desc with
           | Pstr_eval ({ pexp_desc = Pexp_ident { txt = Lident id; _ }; _ }, _) ->
             Some id
           | _ -> None
         in
         (* let str = [ { pstr_desc = Pstr_eval (expr, []); pstr_loc = loc } ] in *)
         hole_noid ~loc ()) *)

(* open module as instances *)
(* let ext_name = instance_name *)

(* open%instance M *)
let open_instance_toplevel =
  Context_free.Rule.extension
  @@ Extension.declare
       instance_name
       Extension.Context.structure_item
       Ast_pattern.(pstr @@ pstr_open __ ^:: nil)
       (fun ~loc ~path:_ odecl ->
         stri_dummy_binding ~loc None { odecl.popen_expr with pmod_attributes = [] })

(* let open%instance M in e *)
let open_instance_local =
  Context_free.Rule.extension
  @@ Extension.declare
       instance_name
       Extension.Context.expression
       Ast_pattern.(pstr @@ pstr_eval (pexp_open __ __) nil ^:: nil)
       (fun ~loc ~path:_ odecl expr -> expr_dummy_binding ~loc None odecl.popen_expr expr)
