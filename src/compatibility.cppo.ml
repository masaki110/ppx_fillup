#if OCAML_VERSION >= (5, 1, 0)

  let lid_of_path path =
    let rec loop =
      Path.(
        function
        | Pident id -> Longident.Lident (Ident.name id)
        | Pdot (p, str) -> Longident.Ldot (loop p, str)
        | Papply (p1, p2) -> Longident.Lapply (loop p1, loop p2)
        | Pextra_ty (p, Pcstr_ty txt) -> Longident.Ldot (loop p, txt)
        | Pextra_ty (p, Pext_ty) -> loop p)
    in
    loop path

# else

  let lid_of_path path =
    let rec loop =
      Path.(
        function
        | Pident id -> Longident.Lident (Ident.name id)
        | Pdot (p, str) -> Longident.Ldot (loop p, str)
        | Papply (p1, p2) -> Longident.Lapply (loop p1, loop p2))
    in
    loop path

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