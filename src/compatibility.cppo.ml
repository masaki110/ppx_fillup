#if OCAML_VERSION >= (4, 14, 0)

  let type_structure env str =
    let tstr, _, _, _, _ = Typemod.type_structure env str in
    tstr

  let match_type env texp texp' =
    Ctype.does_match env texp texp'

  let repr_type env (texp : Types.type_expr) = 
    (* Types.get_desc texp *)
    (* (Types.Transient_expr.repr (Ctype.expand_head env texp)).desc *)
    Types.get_desc (Ctype.expand_head env texp)

#else
  
  let type_structure env str =
    let tstr, _, _, _ = Typemod.type_structure env str in
    tstr

  let repr_type env texp =
    (Ctype.repr (Ctype.expand_head env texp)).desc

  #if OCAML_VERSION >= (4, 13, 0)
    
    let match_type env texp texp' =
      Ctype.does_match env texp texp'
      
  #else
    let match_type env texp texp' =
      Ctype.matches env texp texp'

  #endif

#endif