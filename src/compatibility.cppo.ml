#if OCAML_VERSION >= (4, 14, 0)

  let match_type env texp1 texp2 =
    Ctype.does_match env texp1 texp2
  let repr_type _ (texp : Types.type_expr) = texp

#elif OCAML_VERSION >= (4, 13, 0)

  let match_type env texp1 texp2 =
    Ctype.does_match env texp1 texp2

  let repr_type env texp =
    Ctype.repr (Ctype.expand_head env texp)

#else

  let match_type env texp1 texp2 =
    Ctype.matches env texp1 texp2

  let repr_type env texp =
    Ctype.repr (Ctype.expand_head env texp)

#endif