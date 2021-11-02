(* Reference : 
    https://github.com/ocaml-ppx/ppx_deriving/blob/master/src_plugins/show/ppx_deriving_show.cppo.ml *)

open Parsetree
open Ast_helper

let mkloc (txt:'a) loc = ({ txt; loc }:'a with_loc)

let mknoloc = Location.mknoloc

let evar name = Exp.ident @@ mknoloc (Ppxlib.Longident.parse name)
  
let pvar name = Pat.var @@ mknoloc name

let fold_right_type_decl fn ({ptype_params;_}:type_declaration) accum =
  List.fold_right (fun (param, _) accum ->
    match param with
  | { ptyp_desc = Ptyp_any; _ } -> accum
  | { ptyp_desc = Ptyp_var name; _ } ->
    let name = mkloc name param.ptyp_loc in
    fn name accum
  | _ -> assert false) ptype_params accum

(* generate instance : e.g. let _inst_show_foobar[@instance] = {show=(fun x -> show_foobar x)} *)
let str_of_type plugins ({ptype_loc = loc; _} as type_decl)  =
  let mangle fixpoint name = 
    match fixpoint with
    | `Prefix x -> x ^ "_" ^ name
    | `Suffix x -> name ^ "_" ^ x
  in
  let mangle_type_decl fixpoint =
    let name = type_decl.ptype_name.txt in
    mangle fixpoint name
  in
  let expr_of_string fixpoint = evar @@ mangle_type_decl fixpoint in
  let nonpoly_inner =
    fold_right_type_decl (fun name expr ->
      let name = name.txt in
      [%expr [%e expr] [%e evar ("poly_"^name)]]) type_decl
  in
  let of_enum_expr = [%expr {of_enum=(fun x -> [%e nonpoly_inner @@ expr_of_string (`Suffix "of_enum")] poly_a x)}] in
  let to_enum_expr = [%expr {to_enum=(fun x -> [%e nonpoly_inner @@ expr_of_string (`Suffix "to_enum")] poly_a x)}] in
  let compare_expr = [%expr {compare=(fun x y -> [%e nonpoly_inner @@ expr_of_string (`Prefix "compare")] x y)}] in
  let equal_expr = [%expr {equal=(fun x y -> [%e nonpoly_inner @@ expr_of_string (`Prefix "equal")] x y)}] in
  (* let poly_inner inner =
    let expr = 
      let inner_expr = expr_of_string (`Prefix inner) in
      Ppx_deriving.fold_right_type_decl (fun name expr ->
        let name = name.txt in
        [%expr [%e expr] [%e evar ("poly_"^name)].pp]) type_decl inner_expr
    in
    [%expr {pp=(fun x -> [%e expr] x)}]
  in *)
  let show_expr =
    let expr = 
      let inner_expr = expr_of_string (`Prefix "pp") in
      fold_right_type_decl (fun name expr ->
        let name = name.txt in
        [%expr [%e expr] [%e evar ("poly_"^name)].pp]) type_decl inner_expr
    in
    [%expr {pp=(fun x -> [%e expr] x)}]
  in
  let poly_fun_of_type_decl type_decl expr = (**)
    fold_right_type_decl (fun name expr ->
      let name = name.txt in
      (* Exp.fun_ Nolabel None (pvar ("poly_"^name)) expr *)
      [%expr fun [%p (pvar ("poly_"^name))] -> [%e expr]]
      ) type_decl expr
  in
  let polymorphize = poly_fun_of_type_decl type_decl in
  let pat_of_string str = 
    Pat.var ~attrs:[Attr.mk (mknoloc "instance") (PStr [Str.eval [%expr ()]])]
      @@ mknoloc @@ mangle_type_decl (`Prefix ("_inst_" ^ str))
  in
  let rec check_plugin plugins =
    match plugins with
    | [""] -> 
      [Vb.mk (pat_of_string "show") (polymorphize show_expr);
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

module M = struct
  let get_plugins () =
    match Ocaml_common.Ast_mapper.get_cookie "ppx_deriving" with
    | None -> []
    | Some expr ->
        match Ppxlib_ast.Selected_ast.Of_ocaml.copy_expression expr with
        | { pexp_desc = Pexp_tuple exprs; _} ->
          exprs |> List.map (fun expr ->
            match expr with
            | { pexp_desc = Pexp_constant (Pconst_string (file, _, None)); _} -> file
            | _ -> assert false)
        | _ -> assert false
end

let () =
  let _plugins = M.get_plugins () in
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
  (* prerr_endline "plugins : ";
  prerr_endline @@ string_of_int @@ List.length plugins; *)
  Ppx_deriving.register deriver
