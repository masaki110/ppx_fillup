open Ppx_fillupapi
open Ppxlib
open Ast_helper

(* let open%instance M in e *)
let open_instance_local =
  Context_free.Rule.extension
  @@ Extension.declare
       instance_name
       Extension.Context.expression
       Ast_pattern.(pstr @@ pstr_eval (pexp_open __ __) nil ^:: nil)
       (fun ~loc ~path:_ odecl expr ->
         expr |> Exp.open_ ~loc @@ instantiate_open ~loc None odecl.popen_expr)

(* open%instance M *)
let open_instance_toplevel =
  Context_free.Rule.extension
  @@ Extension.declare
       instance_name
       Extension.Context.structure_item
       Ast_pattern.(pstr @@ pstr_open __ ^:: nil)
       (fun ~loc ~path:_ odecl ->
         Str.open_ ~loc @@ instantiate_open ~loc None odecl.popen_expr)
