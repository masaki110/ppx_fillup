open Parsetree
open Ast_helper

(* Reference : 
    https://github.com/ocaml-ppx/ppx_deriving/blob/master/src_plugins/show/ppx_deriving_show.cppo.ml *)

let instances = [] 

(* generate instance : 
  e.g. let _inst_show_foobar[@instance] = {show=(fun x -> show_foobar x)} *)
let str_of_type _plugin ({ptype_loc = loc; _} as type_decl)  =
  let expr_of_string fix str =
    match fix with
    | `Suffix ->
      Ppx_deriving.Ast_convenience.evar @@ 
        Ppx_deriving.mangle_type_decl (`Suffix str) type_decl
    | `Prefix -> 
      Ppx_deriving.Ast_convenience.evar @@ 
        Ppx_deriving.mangle_type_decl (`Prefix str) type_decl in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let poly_show_expr =
    [%expr fun (inner:'a pp) -> {pp=(fun x -> [%e expr_of_string `Prefix "pp"] inner.pp x)}] in
  let of_enum_expr = [%expr {of_enum=(fun x -> [%e expr_of_string `Suffix "of_enum"] x)}] in
  let to_enum_expr = [%expr {to_enum=(fun x -> [%e expr_of_string `Suffix "to_enum"] x)}] in
  let compare_expr = [%expr {compare=(fun x y -> [%e expr_of_string `Prefix "compare"] x y)}] in
  let equal_expr = [%expr {equal=(fun x y -> [%e expr_of_string `Prefix "equal"] x y)}] in
  let show_expr = [%expr {pp=(fun x -> [%e expr_of_string `Prefix "pp"] x)}] in
  let pat_of_string str =
    Ppx_deriving.Ast_convenience.pvar @@ 
      Ppx_deriving.mangle_type_decl (`Prefix ("_inst_" ^ str)) type_decl in
  match _plugin with
  | "show" ->
    [ Vb.mk (pat_of_string "show") (polymorphize show_expr);
      Vb.mk (pat_of_string "polymorphic_show") (polymorphize poly_show_expr);
      ]
    @instances
  | "equal" -> 
    Vb.mk (pat_of_string "equal") (polymorphize equal_expr)::instances
  | "compare" ->
    Vb.mk (pat_of_string "compare") (polymorphize compare_expr)::instances
  | "enum" ->
    [ Vb.mk (pat_of_string "to_enum") (polymorphize to_enum_expr);
      Vb.mk (pat_of_string "of_enum") (polymorphize of_enum_expr);]
    @instances
  | "" ->
    [ Vb.mk (pat_of_string "show") (polymorphize show_expr);
      Vb.mk (pat_of_string "polymorphic_show") (polymorphize poly_show_expr);
      Vb.mk (pat_of_string "equal") (polymorphize equal_expr);
      Vb.mk (pat_of_string "compare") (polymorphize compare_expr);
      (* Vb.mk (pat_of_string "to_enum") (polymorphize to_enum_expr); *)
      (* Vb.mk (pat_of_string "of_enum") (polymorphize of_enum_expr); *)
      ]
  | _ -> instances


let () =
  (* let deriving_plugins = ["show";"eq";"ord";"enum"] in
  let rec instances plugin_list type_decls =
    match plugin_list with
    | x::xs ->
      begin match Ppx_deriving.lookup x with
      | Some _ -> str_of_type x type_decls
      | None -> instances xs type_decls
      end
    | [] -> []
  in *)
  let deriver = 
    Ppx_deriving.create "fillup"
      ~type_decl_str: 
      (fun ~options:_ ~path:_ type_decls ->
        [Ppxlib.Ast_helper.Str.value 
          Ppxlib.Nonrecursive 
          (* (List.concat (List.map (instances deriving_plugins) type_decls))]) *)
          (List.concat (List.map (str_of_type "") type_decls))])
      ()
  in
  Ppx_deriving.register deriver
