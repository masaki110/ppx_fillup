(* Reference : 
    https://github.com/ocaml-ppx/ppx_deriving/blob/master/src_plugins/show/ppx_deriving_show.cppo.ml *)

open Parsetree
open Ast_helper

let mknoloc = Location.mknoloc

let evar name = Exp.ident @@ mknoloc (Ppxlib.Longident.Lident name)
  
let pvar name = Pat.var @@ mknoloc name

(* generate instance : e.g. let _inst_show_foobar[@instance] = {show=(fun x -> show_foobar x)} *)
let str_of_type plugins ({ptype_loc = loc; _} as type_decl) =
  let fold_right_type_decl = Ppx_deriving.fold_right_type_decl in
  let mangle_type_decl affix = Ppx_deriving.mangle_type_decl affix type_decl in
  let expr_of_string affix = evar @@ mangle_type_decl affix in
  let apply_inner inner =
    match inner with
    | Some "pp" ->
      fold_right_type_decl (fun name expr ->
        let name = name.txt in
        [%expr [%e expr] [%e evar ("poly_"^name)].pp]) type_decl
    | _ -> 
      fold_right_type_decl (fun name expr ->
        let name = name.txt in
        [%expr [%e expr] [%e evar ("poly_"^name)]]) type_decl
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let pat_of_string str = 
    Pat.var ~attrs:[Attr.mk (mknoloc "instance") (PStr [Str.eval [%expr ()]])]
      @@ mknoloc @@ mangle_type_decl (`Prefix ("_inst_" ^ str)) 
  in
  let rec check_plugin plugins =
    match plugins with
    | [""] -> 
      (* let of_enum_expr = [%expr {of_enum=(fun x -> [%e apply_inner None @@ expr_of_string (`Suffix "of_enum")] x)}] in
      let to_enum_expr = [%expr {to_enum=(fun x -> [%e apply_inner None @@ expr_of_string (`Suffix "to_enum")] x)}] in *)
      let compare_expr = [%expr {compare=(fun x y -> [%e apply_inner None @@ expr_of_string (`Prefix "compare")] x y)}] in
      let equal_expr = [%expr {equal=(fun x y -> [%e apply_inner None @@ expr_of_string (`Prefix "equal")] x y)}] in
      let show_expr = [%expr {pp=(fun x -> [%e apply_inner (Some "pp") @@ expr_of_string (`Prefix "pp")] x)}] in
      [Vb.mk (pat_of_string "show") (polymorphize show_expr);
       Vb.mk (pat_of_string "equal") (polymorphize equal_expr);
       Vb.mk (pat_of_string "compare") (polymorphize compare_expr);
      (* Vb.mk (pat_of_string "to_enum") (polymorphize to_enum_expr); *)
      ]
    | x::xs -> 
      begin match x with
      | "show" ->
        let show_expr = [%expr {pp=(fun x -> [%e apply_inner (Some "pp") @@ expr_of_string (`Prefix "pp")] x)}] in
        [Vb.mk 
          (pat_of_string "show") 
          (polymorphize show_expr)]@check_plugin xs
      | "eq" -> 
        let equal_expr = [%expr {equal=(fun x y -> [%e apply_inner None @@ expr_of_string (`Prefix "equal")] x y)}] in
        [Vb.mk (pat_of_string "equal") (polymorphize equal_expr)]@check_plugin xs
      | "ord" ->
        let compare_expr = [%expr {compare=(fun x y -> [%e apply_inner None @@ expr_of_string (`Prefix "compare")] x y)}] in
        [Vb.mk (pat_of_string "compare") (polymorphize compare_expr)]@check_plugin xs
      | "enum" ->
        let of_enum_expr = [%expr {of_enum=(fun x -> [%e apply_inner None @@ expr_of_string (`Suffix "of_enum")] x)}] in
        let to_enum_expr = [%expr {to_enum=(fun x -> [%e apply_inner None @@ expr_of_string (`Suffix "to_enum")] x)}] in
        [Vb.mk (pat_of_string "to_enum") (polymorphize to_enum_expr);
         Vb.mk (pat_of_string "of_enum") (polymorphize of_enum_expr);]@check_plugin xs
      | _ -> []
      end
    | _ -> []
  in
  check_plugin plugins

let () =
  let deriver = 
    Ppx_deriving.create "fillup"
      ~type_decl_str: 
      (fun ~options:_ ~path:_ type_decls ->
        [Str.value 
          Ppxlib.Nonrecursive 
          (* (List.concat (List.map (str_of_type plugins) type_decls))]) *)
          (List.concat (List.map (str_of_type [""]) type_decls))])
      ()
  in
  Ppx_deriving.register deriver
