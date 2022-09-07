#if OCAML_VERSION >= (4, 14, 0)

  let type_structure env str =
    let tstr, _, _, _, _ = Typemod.type_structure env str in
    tstr

  let match_type env texp1 texp2 =
    Ctype.does_match env texp1 texp2

  let repr_type _ (texp : Types.type_expr) = Types.get_desc texp

#else
  
  let type_structure env str =
    let tstr, _, _, _ = Typemod.type_structure env str in
    tstr

    let repr_type env texp =
      (Ctype.repr (Ctype.expand_head env texp)).desc
  
  #if OCAML_VERSION >= (4, 13, 0)
    
    let match_type env texp1 texp2 =
      Ctype.does_match env texp1 texp2
      
  #else
    let match_type env texp1 texp2 =
      Ctype.matches env texp1 texp2

  #endif

#endif