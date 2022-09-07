#if OCAML_VERSION >= (4, 14, 0)

  (* let otyp_of_typ _env typ =
    Printtyp.prepare_for_printing [ typ ];
    Printtyp.tree_of_typexp Type typ

  let type_expr_desc typ =
    (Types.Transient_expr.repr typ).desc

  let type_structure env str =
    let tstr, _, _, _, _ = Typemod.type_structure env str in
    tstr *)

  let match_type env texp1 texp2 =
    Ctype.does_match env texp1 texp2

  let repr_type env (texp : Types.type_expr) = texp

#else

  (* let otyp_of_typ env typ =
    let typ = Ctype.repr (Ctype.expand_head env typ) in
    Printtyp.reset_and_mark_loops typ;
    Printtyp.tree_of_typexp false typ

  let type_expr_desc typ  =
    typ.Types.desc

  let type_structure env str =
    let tstr, _, _, _ = Typemod.type_structure env str in
    tstr *)

  let match_type env texp1 texp2 =
    Ctype.matches env texp1 texp2

  let repr_type env texp =
    Ctype.repr (Ctype.expand_head env texp)

#endif