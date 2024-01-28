open Compatibility
open Ppx_fillupapi

type lpath =
  { level : int
  ; path : Path.t
  }

type instance =
  { id : string
  ; lpath : lpath
  ; desc : Types.value_description
  }

type kinds_instance =
  | Mono of instance
  | Poly of instance

let mono id lpath desc = Mono { id; lpath; desc }
let poly id lpath desc = Poly { id; lpath; desc }

let string_of_instance inst =
  match inst with
  | Mono inst | Poly inst ->
    Format.asprintf
      "(%s) %s : %a"
      inst.id
      (Path.name inst.lpath.path)
      Printtyp.type_expr
      inst.desc.val_type

let string_of_iset = string_of_list string_of_instance

module Typed = struct
  open Parsetree

  let instantiate_deriving env =
    let plugin name attrs =
      let deriver_exprs =
        match find_attr "deriving" attrs with
        | Some
            (PStr
              [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_tuple exprs; _ }, []); _ } ])
          -> Some exprs
        | Some
            (PStr
              [ { pstr_desc =
                    Pstr_eval
                      (({ pexp_desc = Pexp_ident _ | Pexp_apply _; _ } as expr), [])
                ; _
                }
              ]) -> Some [ expr ]
        | _ -> None
      in
      let mangle affix = mangle affix name in
      match deriver_exprs with
      | None -> []
      | Some exprs ->
        List.fold_left
          (fun acc expr ->
            match Pprintast.string_of_expression expr with
            | "show" ->
              ("show", mangle (`Prefix "pp")) :: ("show", mangle (`Prefix "show")) :: acc
            | "eq" -> ("equal", mangle (`Prefix "equal")) :: acc
            | "ord" -> ("compare", mangle (`Prefix "compare")) :: acc
            | "sexp" ->
              ("sexp_of", mangle (`Prefix "sexp_of"))
              :: ("of_sexp", mangle (`Suffix "of_sexp"))
              :: acc
            | _ -> acc)
          []
          exprs
    in
    Env.fold_types
      (fun name _ tdecl acc -> acc @ plugin name tdecl.type_attributes)
      None
      env
      []

  (*** wether texp has INSTANCE ***)
  let make_iset (texp : T.expression) =
    let env = texp.exp_env in
    let hid = id_of_texp texp in
    (* Get HOLE ident *)
    let instantiate full_instantiate path (desc : Types.value_description) =
      let lpath = { level = 0; path } in
      (*** override instantiated id ***)
      match full_instantiate with
      | None ->
        (match find_attr instance_name desc.val_attributes with
         | Some pl -> if hid = id_of_payload pl then Some (mono hid lpath desc) else None
         | None ->
           (match find_attr rec_instance_name desc.val_attributes with
            | Some pl ->
              if hid = id_of_payload pl then Some (poly hid lpath desc) else None
            | None -> None))
      | Some id ->
        if id <> hid
        then None
        else (
          match find_attr rec_instance_name desc.val_attributes with
          | Some _ -> Some (poly hid lpath desc)
          | None -> Some (mono hid lpath desc))
    in
    (*** instantiate values in module ***)
    let instance_from_mdecl path mdecl =
      let open Types in
      let idopt = find_attr instance_name mdecl.md_attributes >>= idopt_of_payload in
      let rec loop path mdecl =
        match mdecl.md_type with
        | Mty_ident path | Mty_alias path ->
          let mdecl =
            try Env.find_module path env with
            | Not_found -> raise_errorf "Not found: %s" (Path.name path)
          in
          loop path mdecl
        | Mty_signature sg ->
          List.fold_left
            (fun acc -> function
              | Types.Sig_value (ident, vdesc, _) ->
                (match instantiate idopt (Path.Pdot (path, Ident.name ident)) vdesc with
                 | Some i -> i :: acc
                 | None -> acc)
              | _ -> acc)
            []
            sg
        | Mty_functor _ -> []
      in
      loop path mdecl
    in
    let instantiate_module =
      Env.fold_modules
        (fun name path mdecl acc ->
          if Str.(string_match (regexp dummy_mprefix) name 0)
          then instance_from_mdecl path mdecl @ acc
          else acc)
        None
        env
        []
    in
    let hole_id : string option ref = ref None in
    let search_envvalues name path desc acc =
      let is_hole =
        match hid, find_attr overload_name desc.Types.val_attributes with
        | _, None -> None
        | hname, Some _ -> if name = hname then Some hid else None
      in
      let match_id =
        let rec loop = function
          | [] -> None
          | (id, derived_name) :: rest ->
            if name = derived_name then Some id else loop rest
        in
        loop
      in
      match is_hole, instantiate None path desc, match_id (instantiate_deriving env) with
      | Some id, None, None ->
        (*** 'texp' is HOLE ident ***)
        hole_id := Some id;
        acc
      | None, Some i, None ->
        (*** Instantiate value in env ***)
        i :: acc
      | None, None, Some id ->
        (*** Instantiate 'deriving' functions ***)
        Mono { id; lpath = { level = 0; path }; desc } :: acc
      | _, _, _ -> acc
    in
    (*** Whether texp has [@HOLE id] ***)
    let iset = instantiate_module @ Env.fold_values search_envvalues None env [] in
    match !hole_id with
    | None -> raise Not_hole
    | Some _ -> iset

  (*** HOLE & INSTANCE type-match ***)
  let type_match (hole : T.expression) =
    let match_instance env hole = function
      | Poly { id; lpath; desc } ->
        let rec loop path texp =
          (* let matches hole texp = match_type env hole texp || match_type env texp hole in *)
          if match_type env hole texp || match_type env texp hole
          then Some { level = 0; path }
          else (
            match repr_type env texp with
            | Types.Tarrow (_, _, ret, _) ->
              (*** Whether INSTANCE is more general HOLE => match_type env hole texp ***)
              loop path ret >>= fun lpath -> Some { lpath with level = lpath.level + 1 }
            | _ ->
              (* if match_type env hole texp || match_type env texp hole
            then Some { level = 0; path }
            else *)
              None)
        in
        loop lpath.path desc.val_type >>= fun lpath -> Some (Poly { id; lpath; desc })
      | Mono { desc; _ } as inst ->
        (*** Whether HOLE is more general INSTANCE => match_type env texp hole ***)
        if match_type env desc.val_type hole (* || match_type env hole desc.val_type *)
        then Some inst
        else None
    in
    let rec loop = function
      | inst :: rest ->
        (match match_instance hole.exp_env hole.exp_type inst with
         | Some i -> i :: loop rest
         | None -> loop rest)
      | [] -> []
    in
    loop (make_iset hole)

  let apply_holes n hole expr =
    let hole =
      match hole.T.exp_desc with
      | Texp_ident (_, lid, _) -> Ast_helper.(Exp.ident ~loc:hole.exp_loc lid)
      | _ -> raise Not_hole
    in
    let rec loop n (expr : expression) =
      if n = 0
      then expr
      else loop (n - 1) { expr with pexp_desc = Pexp_apply (expr, [ Nolabel, hole ]) }
    in
    loop n expr

  (*** Replace hole to instance ***)
  let replace_hole (super : Untypeast.mapper) self texp =
    let loc, attrs = texp.T.exp_loc, texp.exp_attributes in
    (*** HOLE & INSTANCE type-match ***)
    match uniq @@ type_match texp with
    | exception Not_hole -> super.expr self texp
    (*** Match only one ***)
    | [ Mono { lpath; _ } ] | [ Poly { lpath; _ } ] ->
      apply_holes lpath.level texp
      @@ Ast_helper.Exp.ident ~loc ~attrs
      @@ mknoloc
      @@ lident_of_path lpath.path
    (*** Match two or more ***)
    | _ :: _ as l ->
      raise_errorf
        ~loc
        "(ppx_fillup) Instance overlapped: %a \n %s "
        Printtyp.type_scheme
        texp.exp_type
        (string_of_iset l)
    (*** No match ***)
    | [] ->
      raise_errorf
        ~loc
        "(ppx_fillup) Instance not found: %a"
        Printtyp.type_scheme
        texp.exp_type

  let untyper f =
    let super = default_untyper in
    let self = { super with expr = f super } in
    self.structure self

  let fillup str =
    Compmisc.init_path ();
    let env = Compmisc.initial_env () in
    let loc = Location.none in
    let hide_warn20 = To_ocaml.structure_item [%stri [@@@warning "-20"]] in
    let str = hide_warn20 :: str in
    let rec loop str =
      (* Format.eprintf "\n%a\n" Pprintast.structure str; *)
      let tstr = type_structure env str in
      let str' = untyper replace_hole tstr in
      if str = str'
      then
        (function
          | h :: t when h = hide_warn20 -> t
          | str -> str)
          str'
      else loop str'
    in
    loop str
end

module Untyped = struct
  open Ppxlib
  open! Parsetree
  open Ast_helper

  class preprocess =
    object (this)
      inherit Ppxlib.Ast_traverse.map as super

      method transform_rec_instance id expr =
        let loc = expr.pexp_loc in
        let l = ref [] in
        let replace_id expr =
          let rec mapper expr =
            let return e = Some (mapper e) in
            let mapping pexp_desc = { expr with pexp_desc } in
            match expr.pexp_desc with
            (*** replace identifier overloading to add_arg's argument ***)
            | Pexp_ident { txt = Lident hid; _ } ->
              if hid = id
              then (
                let varname = mk_dummy_vname () in
                l := varname :: !l;
                mapping @@ Pexp_ident { txt = Lident varname; loc })
              else expr
            | Pexp_let (a, vbs, ret) ->
              mapping
              @@ Pexp_let
                   ( a
                   , List.map (fun vb -> { vb with pvb_expr = mapper vb.pvb_expr }) vbs
                   , mapper ret )
            | Pexp_function l ->
              mapping
              @@ Pexp_function (List.map (fun c -> { c with pc_rhs = mapper c.pc_rhs }) l)
            | Pexp_fun (a, b, c, ret) -> mapping @@ Pexp_fun (a, b, c, mapper ret)
            | Pexp_apply (expr, l) ->
              mapping
              @@ Pexp_apply (mapper expr, List.map (fun (lbl, expr) -> lbl, mapper expr) l)
            | Pexp_match (e, l) ->
              mapping
              @@ Pexp_match
                   (mapper e, List.map (fun c -> { c with pc_rhs = mapper c.pc_rhs }) l)
            | Pexp_try (e, l) ->
              mapping
              @@ Pexp_try
                   (mapper e, List.map (fun c -> { c with pc_rhs = mapper c.pc_rhs }) l)
            | Pexp_tuple l -> mapping @@ Pexp_tuple (List.map mapper l)
            | Pexp_construct (a, opt) -> mapping @@ Pexp_construct (a, opt >>= return)
            | Pexp_variant (a, opt) -> mapping @@ Pexp_variant (a, opt >>= return)
            | Pexp_record (l, opt) ->
              mapping
              @@ Pexp_record (List.map (fun (a, e) -> a, mapper e) l, opt >>= return)
            | Pexp_field (e, a) -> mapping @@ Pexp_field (mapper e, a)
            | Pexp_setfield (e1, a, e2) ->
              mapping @@ Pexp_setfield (mapper e1, a, mapper e2)
            | Pexp_array l -> mapping @@ Pexp_array (List.map mapper l)
            | Pexp_ifthenelse (e1, e2, opt) ->
              mapping @@ Pexp_ifthenelse (mapper e1, mapper e2, opt >>= return)
            | Pexp_sequence (e1, e2) -> mapping @@ Pexp_sequence (mapper e1, mapper e2)
            | Pexp_while (e1, e2) -> mapping @@ Pexp_while (mapper e1, mapper e2)
            | Pexp_for (a, e1, e2, b, e3) ->
              mapping @@ Pexp_for (a, mapper e1, mapper e2, b, mapper e3)
            | Pexp_constraint (e, a) -> mapping @@ Pexp_constraint (mapper e, a)
            | Pexp_coerce (e, a, b) -> mapping @@ Pexp_coerce (mapper e, a, b)
            | Pexp_send (e, a) -> mapping @@ Pexp_send (mapper e, a)
            | Pexp_setinstvar (a, e) -> mapping @@ Pexp_setinstvar (a, mapper e)
            | Pexp_override l ->
              mapping @@ Pexp_override (List.map (fun (a, e) -> a, mapper e) l)
            | Pexp_letmodule (a, b, e) -> mapping @@ Pexp_letmodule (a, b, mapper e)
            | Pexp_letexception (a, e) -> mapping @@ Pexp_letexception (a, mapper e)
            | Pexp_assert e -> mapping @@ Pexp_assert (mapper e)
            | Pexp_lazy e -> mapping @@ Pexp_lazy (mapper e)
            | Pexp_poly (e, a) -> mapping @@ Pexp_poly (mapper e, a)
            | Pexp_newtype (a, e) -> mapping @@ Pexp_newtype (a, e)
            | Pexp_open (a, e) -> mapping @@ Pexp_open (a, mapper e)
            | Pexp_letop { let_; ands; body } ->
              mapping
              @@ Pexp_letop
                   { let_
                   ; ands =
                       List.map
                         (fun and_ -> { and_ with pbop_exp = mapper and_.pbop_exp })
                         ands
                   ; body = mapper body
                   }
            | _ -> expr
          in
          mapper expr
        in
        (*** Add alternative argument for identifier overloading ***)
        let rec add_args expr = function
          | [] -> expr
          | txt :: rest ->
            add_args (Exp.fun_ ~loc Nolabel None (Pat.var { txt; loc }) expr) rest
        in
        let expr = replace_id expr in
        add_args expr !l

      (* let pat[@instance] = e
         ==> let __fillup1[@instance pat] = e and pat[@overload] = magic () : _ *)
      method rename_instance vbs =
        let rec loop_vbs acc = function
          | [] -> List.rev acc
          | ({ pvb_pat = pat; pvb_expr; _ } as vb) :: rest ->
            let loc, attrs = pat.ppat_loc, pat.ppat_attributes in
            let find_attrs attrs =
              let rec loop_attrs acc = function
                | [] -> raise Not_instance
                | attr :: rest ->
                  let id_name =
                    match pat.ppat_desc with
                    | Ppat_var str -> str.txt
                    | _ -> raise Not_instance
                  in
                  let id_binding =
                    { vb with
                      pvb_pat =
                        { pat with
                          ppat_attributes =
                            (List.rev
                             @@ ({ attr with
                                   attr_name = mkloc ~loc overload_name
                                 ; attr_payload = PStr []
                                 }
                                 :: acc))
                            @ rest
                        }
                    ; pvb_expr = voidexpr ~loc ()
                    }
                  in
                  let id_definition =
                    { vb with
                      pvb_pat =
                        { pat with
                          ppat_desc = Ppat_var (mkloc ~loc (mk_dummy_vname ()))
                        ; ppat_attributes =
                            (List.rev
                             @@ ({ attr with
                                   attr_payload =
                                     PStr
                                       [ Str.eval
                                           ~loc
                                           (Exp.ident { txt = Lident id_name; loc })
                                       ]
                                 }
                                 :: acc))
                            @ rest
                        }
                    }
                  in
                  (match
                     ( attr.attr_name.txt = instance_name
                     , attr.attr_name.txt = rec_instance_name )
                   with
                   | true, false -> [ id_binding; id_definition ]
                   | false, true ->
                     [ id_binding
                     ; { id_definition with
                         pvb_expr = this#transform_rec_instance id_name pvb_expr
                       }
                     ]
                   | _, _ -> loop_attrs (attr :: acc) rest)
              in
              loop_attrs [] attrs
            in
            (try loop_vbs (find_attrs attrs @ acc) rest with
             | Not_instance -> loop_vbs (vb :: acc) rest)
        in
        loop_vbs [] vbs

      method! expression expr =
        let loc = expr.pexp_loc in
        match expr.pexp_desc with
        | Pexp_let (flag, vbs, expr) ->
          super#expression
            { expr with pexp_desc = Pexp_let (flag, this#rename_instance vbs, expr) }
        (*** Generate 'id' binding when instantiate module ***)
        | Pexp_open ({ popen_expr; _ }, rest) ->
          (match
             find_attr' instance_name popen_expr.pmod_attributes >>= idopt_of_payload'
           with
           | None -> super#expression expr
           | Some id ->
             super#expression @@ rest
             |> Exp.open_ ~loc @@ instantiate_open ~loc id popen_expr)
        | _ -> super#expression expr

      method! structure_item stri =
        let loc = stri.pstr_loc in
        match stri.pstr_desc with
        | Pstr_value (flag, vbs) ->
          super#structure_item
            { stri with pstr_desc = Pstr_value (flag, this#rename_instance vbs) }
        (*** Generate 'id' binding when instantiate module ***)
        | Pstr_open { popen_expr; _ } ->
          (match
             find_attr' instance_name popen_expr.pmod_attributes >>= idopt_of_payload'
           with
           | None -> super#structure_item stri
           | Some id ->
             super#structure_item @@ Str.open_ ~loc @@ instantiate_open ~loc id popen_expr)
        | _ -> super#structure_item stri
    end

  class postprocess =
    object (this)
      inherit Ppxlib.Ast_traverse.map as super

      method remove_id_binding vbs =
        let rec loop acc = function
          | [] -> List.rev acc
          | ({ pvb_pat; _ } as vb) :: rest ->
            (match find_attr' overload_name pvb_pat.ppat_attributes with
             | None -> loop (vb :: acc) rest
             | Some _ -> loop acc rest)
        in
        loop [] vbs

      method! expression expr =
        match expr.pexp_desc with
        | Pexp_let (flag, vbs, rest) ->
          (***  Remove id binding ***)
          super#expression
            { expr with pexp_desc = Pexp_let (flag, this#remove_id_binding vbs, rest) }
        (* | _ when find_attr' "HOLE" expr.pexp_attributes <> None ->
           raise_errorf ~loc:expr.pexp_loc "(ppx_fillup) Failure of instance solve" *)
        | _ -> super#expression expr

      method! structure_item stri =
        let loc = stri.pstr_loc in
        match stri.pstr_desc with
        | Pstr_value (flag, vbs) ->
          (***  Remove id binding ***)
          let vbs' = this#remove_id_binding vbs in
          (match vbs' with
           | [] -> [%stri ()]
           | vbs' ->
             super#structure_item { stri with pstr_desc = Pstr_value (flag, vbs') })
        | _ -> super#structure_item stri
    end

  (*** Transform structure ***)
  let transform (str : Parsetree.structure) =
    if Ocaml_common.Ast_mapper.tool_name () = "ocamldoc"
       || Ocaml_common.Ast_mapper.tool_name () = "ocamldep"
    then (* avoid typer *)
      (new preprocess)#structure str |> (new postprocess)#structure
    else (
      let str =
        (new preprocess)#structure str
        |> To_ocaml.structure
        (* |> expr_mapper Typed.alert_filled *)
        |> Typed.fillup
        |> Of_ocaml.structure
        |> (new postprocess)#structure
      in
      (* Format.eprintf "\n%a\n" Pprintast.structure str; *)
      str)
end
