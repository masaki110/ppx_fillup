open Parsetree
open Ast_helper

let pvar = Ppx_deriving.Ast_convenience.pvar
let evar = Ppx_deriving.Ast_convenience.evar
(* Reference : 
    https://github.com/ocaml-ppx/ppx_deriving/blob/master/src_plugins/show/ppx_deriving_show.cppo.ml *)

(* generate instance : 
  e.g. let _inst_show_foobar[@instance] = {show=(fun x -> show_foobar x)} *)
let str_of_type plugins ({ptype_loc = loc; _} as type_decl)  =
  let expr_of_string fix str =
    match fix with
    | `Suffix ->
      evar @@ Ppx_deriving.mangle_type_decl (`Suffix str) type_decl
    | `Prefix -> 
      evar @@ Ppx_deriving.mangle_type_decl (`Prefix str) type_decl in
  let poly_inner =
    Ppx_deriving.fold_right_type_decl (fun name expr ->
      let name = name.txt in
      [%expr [%e expr] [%e evar ("poly_"^name)]]) type_decl in
  let of_enum_expr = [%expr {of_enum=(fun x -> [%e poly_inner @@ expr_of_string `Suffix "of_enum"] poly_a x)}] in
  let to_enum_expr = [%expr {to_enum=(fun x -> [%e poly_inner @@ expr_of_string `Suffix "to_enum"] poly_a x)}] in
  let compare_expr = [%expr {compare=(fun x y -> [%e poly_inner @@ expr_of_string `Prefix "compare"] x y)}] in
  let equal_expr = [%expr {equal=(fun x y -> [%e poly_inner @@ expr_of_string `Prefix "equal"] x y)}] in
  let show_expr = 
    let expr = 
      let expr = expr_of_string `Prefix "pp" in
      Ppx_deriving.fold_right_type_decl (fun name expr ->
        let name = name.txt in
        [%expr [%e expr] [%e evar ("poly_"^name)].pp]) type_decl expr
    in
    [%expr {pp=(fun x -> [%e expr] x)}] in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let pat_of_string str = 
    Ast_helper.Pat.var ~attrs:[Attr.mk (Location.mknoloc "instance") (PStr [Ast_helper.Str.eval [%expr ()]])]
      @@ Location.mknoloc @@ Ppx_deriving.mangle_type_decl (`Prefix ("_inst_" ^ str)) type_decl in
  (* let _plugins = plugins in 
  let _of_enum_expr = of_enum_exprin
  let _to_enum_expr = to_enum_expr in
  [ Vb.mk (pat_of_string "show") (polymorphize show_expr);
  (* Vb.mk (pat_of_string "polymorphic_show") (polymorphize poly_show_expr); *)
  Vb.mk (pat_of_string "equal") (polymorphize equal_expr);
  Vb.mk (pat_of_string "compare") (polymorphize compare_expr);
  (* Vb.mk (pat_of_string "to_enum") (polymorphize to_enum_expr); *)
  (* Vb.mk (pat_of_string "of_enum") (polymorphize of_enum_expr); *)
  ]  *)
  let rec check_plugin plugins =
    (* let mk id body = Vb.mk ~attrs:[Attr.mk (Location.mknoloc "instance") (PStr [Ast_helper.Str.eval [%expr ()]])] id body in *)
    match plugins with
    | [""] -> 
      [Vb.mk (pat_of_string "show") (polymorphize show_expr);
      (* Vb.mk (pat_of_string "polymorphic_show") (polymorphize poly_show_expr); *)
       Vb.mk (pat_of_string "equal") (polymorphize equal_expr);
       Vb.mk (pat_of_string "compare") (polymorphize compare_expr);
      (* Vb.mk (pat_of_string "to_enum") (polymorphize to_enum_expr); *)
      (* Vb.mk (pat_of_string "of_enum") (polymorphize of_enum_expr); *)
      ]
    | x::xs -> 
      begin match x with
      | "show" ->
        [Vb.mk (pat_of_string "show") (polymorphize show_expr)]@check_plugin xs
      | "eq" -> 
        [Vb.mk (pat_of_string "equal") (polymorphize equal_expr)]@check_plugin xs
      | "ord" ->
        [Vb.mk (pat_of_string "compare") (polymorphize compare_expr)]@check_plugin xs
      | "enum" ->
        [Vb.mk (pat_of_string "to_enum") (polymorphize to_enum_expr);
         Vb.mk (pat_of_string "of_enum") (polymorphize of_enum_expr);]@check_plugin xs
      | _ -> []
      end
    | _ -> []
  in
  check_plugin plugins

let get_plugins () =
  match Ocaml_common.Ast_mapper.get_cookie "ppx_deriving" with
  | None -> []
  | Some expr ->
      match Ppxlib_ast__.Import.Selected_ast.Of_ocaml.copy_expression expr with
      | { pexp_desc = Pexp_tuple exprs; _} ->
        exprs |> List.map (fun expr ->
          match expr with
          | { pexp_desc = Pexp_constant (Pconst_string (file, _, None)); _} -> file
          | _ -> assert false)
      | _ -> assert false

let () =
  let plugins = get_plugins () in
  let deriver = 
    Ppx_deriving.create "fillup"
      ~type_decl_str: 
      (fun ~options:_ ~path:_ type_decls ->
        [Ppxlib.Ast_helper.Str.value 
          Ppxlib.Nonrecursive 
          (* (List.concat (List.map (str_of_type plugins) type_decls))]) *)
          (List.concat (List.map (str_of_type [""]) type_decls))])
      ()
  in
  List.iter (prerr_endline) plugins;
  Ppx_deriving.register deriver
