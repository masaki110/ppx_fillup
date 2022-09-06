[@@@warnerror "-33"]

(* Reference :
    https://github.com/ocaml-ppx/ppx_deriving/blob/master/src_plugins/show/ppx_deriving_show.cppo.ml *)

open Parsetree
open Ast_helper

let mknoloc = Location.mknoloc
let evar name = Exp.ident @@ mknoloc (Ppxlib.Longident.Lident name)
let pvar name = Pat.var @@ mknoloc name

let expr_of_string type_decl affix =
  evar @@ Ppx_deriving.mangle_type_decl affix type_decl

let apply_inner loc type_decl inner =
  match inner with
  | Some "pp" ->
      Ppx_deriving.fold_right_type_decl
        (fun name expr ->
          let name = name.txt in
          [%expr [%e expr] [%e evar ("poly_" ^ name)].pp])
        type_decl
  | _ ->
      Ppx_deriving.fold_right_type_decl
        (fun name expr ->
          let name = name.txt in
          [%expr [%e expr] [%e evar ("poly_" ^ name)]])
        type_decl

let pat_of_string loc type_decl affix =
  Pat.var ~attrs:[ Attr.mk (mknoloc "instance") (PStr [ Str.eval [%expr ()] ]) ]
  @@ mknoloc
  @@ "inst_"
  ^ Ppx_deriving.mangle_type_decl affix type_decl

(* generate instance : e.g. let _inst_show_foobar[@instance] = {show=(fun x -> show_foobar x)} *)
let str_of_type plugins ({ ptype_loc = loc; _ } as type_decl) =
  let expr_of_string = expr_of_string type_decl in
  let apply_inner = apply_inner loc type_decl in
  let pat_of_string = pat_of_string loc type_decl in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let rec check_plugin plugins =
    match plugins with
    | x :: xs -> (
        match x with
        | [%expr fillup] -> check_plugin xs
        | [%expr show] ->
            [
              Vb.mk
                (pat_of_string (`Prefix "show"))
                (polymorphize
                   [%expr
                     {
                       pp =
                         (fun x ->
                           [%e
                             apply_inner (Some "pp")
                             @@ expr_of_string (`Prefix "pp")]
                             x);
                     }]);
            ]
            @ check_plugin xs
        | [%expr eq] ->
            [
              Vb.mk
                (pat_of_string (`Prefix "equal"))
                (polymorphize
                   [%expr
                     {
                       equal =
                         (fun x y ->
                           [%e
                             apply_inner None
                             @@ expr_of_string (`Prefix "equal")]
                             x y);
                     }]);
            ]
            @ check_plugin xs
        | [%expr ord] ->
            [
              Vb.mk
                (pat_of_string (`Prefix "compare"))
                (polymorphize
                   [%expr
                     {
                       compare =
                         (fun x y ->
                           [%e
                             apply_inner None
                             @@ expr_of_string (`Prefix "compare")]
                             x y);
                     }]);
            ]
            @ check_plugin xs
        | [%expr enum] ->
            [
              Vb.mk
                (pat_of_string (`Suffix "to_enum"))
                (polymorphize
                   [%expr
                     {
                       to_enum =
                         (fun x ->
                           [%e
                             apply_inner None
                             @@ expr_of_string (`Suffix "to_enum")]
                             x);
                     }]);
              Vb.mk
                (pat_of_string (`Suffix "of_enum"))
                (polymorphize
                   [%expr
                     {
                       of_enum =
                         (fun x ->
                           [%e
                             apply_inner None
                             @@ expr_of_string (`Suffix "of_enum")]
                             x);
                     }]);
            ]
            @ check_plugin xs
        | _ -> [])
    | [] -> []
  in
  check_plugin plugins

let derivers_type_decl typ_decls =
  let attributes =
    List.concat
      (List.map (fun { ptype_attributes = attrs; _ } -> attrs) typ_decls)
  in
  let raise_errorf = Location.raise_errorf in
  let deriving = Ppx_deriving.Ast_convenience.find_attr "deriving" attributes in
  let deriver_exprs =
    match deriving with
    | Some
        (PStr
          [
            {
              pstr_desc = Pstr_eval ({ pexp_desc = Pexp_tuple exprs; _ }, []);
              _;
            };
          ]) ->
        exprs
    | Some
        (PStr
          [
            {
              pstr_desc =
                Pstr_eval
                  (({ pexp_desc = Pexp_ident _ | Pexp_apply _; _ } as expr), []);
              _;
            };
          ]) ->
        [ expr ]
    | _ -> raise_errorf "Unrecognized [@@deriving] annotation syntax"
  in
  deriver_exprs

let () =
  let deriver =
    Ppx_deriving.create "fillup"
      ~type_decl_str:(fun ~options:_ ~path:_ type_decls ->
        let plugins = derivers_type_decl type_decls in
        [
          Str.value Ppxlib.Nonrecursive
            (List.concat (List.map (str_of_type plugins) type_decls));
        ])
      ()
  in
  Ppx_deriving.register deriver