#if OCAML_VERSION >= (5, 1, 0)

  let rec lident_of_path = function
  | Path.Pident id -> Longident.Lident (Ident.name id)
  | Path.Papply (p1, p2) ->
      Longident.Lapply (lident_of_path p1, lident_of_path p2)
  | Path.Pdot (p, s) | Path.Pextra_ty (p, Pcstr_ty s) ->
      Longident.Ldot (lident_of_path p, s)
  | Path.Pextra_ty (p, _) -> lident_of_path p

# else

  let rec lident_of_path = function
  | Path.Pident id -> Longident.Lident (Ident.name id)
  | Path.Papply (p1, p2) ->
      Longident.Lapply (lident_of_path p1, lident_of_path p2)
  | Path.Pdot (p, s) ->
      Longident.Ldot (lident_of_path p, s)

#endif


#if OCAML_VERSION >= (4, 14, 0)

  let type_structure env str =
    let tstr, _, _, _, _ = Typemod.type_structure env str in
    tstr

  let repr_type env (texp : Types.type_expr) = 
    Types.get_desc (Ctype.expand_head env texp)

#else

  let type_structure env str =
    let tstr, _, _, _ = Typemod.type_structure env str in
    tstr

  let repr_type env texp =
    (Ctype.repr (Ctype.expand_head env texp)).desc

#endif


#if OCAML_VERSION >= (4, 13, 0)
    
  let match_type env texp texp' =
    Ctype.does_match env texp texp'
    
#else

  let match_type env texp texp' =
    Ctype.matches env texp texp'

#endif