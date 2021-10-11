open Ast_helper
open Parsetree

(* let show_type_of_decl ~options ~path type_decl =
  let loc = type_decl.ptype_loc in
  let _ = parse_options options in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  Ppx_deriving.poly_arrow_of_type_decl
    (fun var -> [%type: Ppx_deriving_runtime.Format.formatter -> [%t var] -> Ppx_deriving_runtime.unit])
    type_decl
    [%type: [%t typ] -> Ppx_deriving_runtime.string] *)

let str_of_type ~options:_ ~path:_ _type_decl =
  let loc = Location.none in
    [Vb.mk [%pat? my_deriver] [%expr "here"]]
  (* let show_type =
    Ppx_deriving.strong_type_of_type @@
      show_type_of_decl ~options ~path type_decl in
  [Vb.mk (Pat.constraint_ show_type) [%expr 1]] *)

let () =
  let deriver = 
    Ppx_deriving.create "show_fill"
      ~type_decl_str: 
      (fun ~options ~path type_decls ->
        [Ppxlib.Ast_helper.Str.value Ppxlib.Nonrecursive (List.concat (List.map (str_of_type ~options ~path) type_decls))])
      ()
  in
  Ppx_deriving.register deriver
