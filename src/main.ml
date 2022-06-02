(* open Parsetree *)
open Ppxlib
open Util

(* Declaration of extension [%HOLE] *)
let hole =
  Extension.declare "HOLE" Extension.Context.expression
    Ast_pattern.(pstr nil)
    (fun ~loc ~path:_ -> Fillup.mkhole ~loc)

(* open%fillup M --> module Dummy = M;; open Dummy*)
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

(* let open%fillup M in e --> let module Dummy = M in e  *)
let open_instance_local =
  Extension.declare "fillup" Extension.Context.expression
    Ast_pattern.(pstr @@ pstr_eval (pexp_open __ __) nil ^:: nil)
    (fun ~loc ~path:_ open_module expr ->
      let mod_exp = open_module.popen_expr in
      let dummy_name = mk_dummy_module () in
      {
        pexp_desc = Pexp_letmodule (mkloc ~loc @@ Some dummy_name, mod_exp, expr);
        pexp_loc = loc;
        pexp_loc_stack = [];
        pexp_attributes = [];
      })

let () =
  Driver.register_transformation
    ~extensions:[ hole; open_instance_toplevel; open_instance_local ]
    ~instrument:(Driver.Instrument.make ~position:After Fillup.transform)
    "ppx_fillup"
