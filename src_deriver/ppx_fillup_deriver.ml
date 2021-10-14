open Parsetree
open Ast_helper

(* Reference : 
    https://github.com/ocaml-ppx/ppx_deriving/blob/master/src_plugins/show/ppx_deriving_show.cppo.ml *)

let show_type_of_decl ~options:_ ~path:_ type_decl =
  let loc = type_decl.ptype_loc in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  Ppx_deriving.poly_arrow_of_type_decl
    (fun var -> 
      [%type: Ppx_deriving_runtime.Format.formatter -> 
        [%t var] -> Ppx_deriving_runtime.unit])
    type_decl
    [%type: [%t typ] -> Ppx_deriving_runtime.string]

(* generate instance : 
  e.g. left-hand : _inst_show_foobar[@instance]
       right-hand : {show=(fun x -> show_foobar x)} *)
let str_of_type ~options:_ ~path:_ ({ptype_loc = loc; _} as type_decl) =
  (* right-hand *)
  let expr_of_str s =
    Ppx_deriving.Ast_convenience.evar @@ 
      Ppx_deriving.mangle_type_decl (`Prefix s) type_decl in
  let equal_expr =
    [%expr {equal=(fun x y -> [%e expr_of_str "equal"] x y)}] in
  let show_expr = 
    [%expr {show=(fun x -> [%e expr_of_str "show"] x)}] in
  let polymorphize  = Ppx_deriving.poly_fun_of_type_decl type_decl in
  (* left-hand *)
  let pat_of_str s =
    Ppx_deriving.Ast_convenience.pvar @@ 
      Ppx_deriving.mangle_type_decl (`Prefix ("_inst_" ^ s)) type_decl in
  [Vb.mk (pat_of_str "show") (polymorphize @@ show_expr);
   Vb.mk (pat_of_str "equal") (polymorphize @@ equal_expr)]

let () =
  let deriver = 
    Ppx_deriving.create "fillup"
      ~type_decl_str: 
      (fun ~options ~path type_decls ->
        [Ppxlib.Ast_helper.Str.value 
          Ppxlib.Nonrecursive 
          (List.concat (List.map (str_of_type ~options ~path) type_decls))])
      ()
  in
  Ppx_deriving.register deriver
