open Compatibility
open Ppx_fillupapi
open Parsetree

let overload_name = "overload"

type lpath =
  { level : int
  ; path : Path.t
  }

type instance =
  { lpath : lpath
  ; desc : Types.value_description
  ; id : string option
  }

type ctx_instance =
  | Mono of instance
  | Poly of instance

let mk_mono lpath desc id = Mono { lpath; desc; id }
let mk_poly lpath desc id = Poly { lpath; desc; id }

let string_of_instance inst =
  match inst with
  | Mono inst | Poly inst ->
    Format.asprintf
      "%s : %a"
      (Path.name inst.lpath.path)
      Printtyp.type_expr
      inst.desc.val_type

let string_of_iset = string_of_list string_of_instance

module Typed = struct
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
            | "show" -> mangle (`Prefix "pp") :: mangle (`Prefix "show") :: acc
            | "eq" -> mangle (`Prefix "equal") :: acc
            | "ord" -> mangle (`Prefix "compare") :: acc
            | "sexp" -> mangle (`Prefix "sexp_of") :: mangle (`Suffix "of_sexp") :: acc
            | _ -> acc)
          []
          exprs
    in
    Env.fold_types
      (fun name _ tdecl acc -> acc @ plugin name tdecl.type_attributes)
      None
      env
      []

  let make_iset (texp : T.expression) =
    let env = texp.exp_env in
    (* Get HOLE ident *)
    let instantiate idopt path (desc : Types.value_description) =
      let lpath = { level = 0; path } in
      (*** override instantiated id ***)
      match idopt with
      | None ->
        (match find_attr instance_name desc.val_attributes with
         | Some pl -> Some (mk_mono lpath desc (id_of_payload pl))
         | None ->
           (match find_attr instance_with_ctxt_name desc.val_attributes with
            | Some pl -> Some (mk_poly lpath desc (id_of_payload pl))
            | None -> None))
      | Some id ->
        (match find_attr instance_with_ctxt_name desc.val_attributes with
         | Some _ -> Some (mk_poly lpath desc id)
         | None -> Some (mk_mono lpath desc id))
    in
    (*** instantiate values in module ***)
    let instance_from_mdecl path mdecl =
      let open Types in
      let idopt =
        try find_attr instance_name mdecl.md_attributes >>= idopt_of_payload with
        | Invalid_payload pl ->
          raise_errorf "(ppx_fillup) Illigal INSTANCE payload: %s" (string_of_payload pl)
      in
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
    let instantiate_mvalue =
      Env.fold_modules
        (fun name path mdecl acc ->
          if Str.(string_match (regexp dummy_prefix) name 0)
          then instance_from_mdecl path mdecl @ acc
          else acc)
        None
        env
        []
    in
    let get_id =
      let hole_payload =
        let find_attr = find_attr hole_name in
        match texp.exp_attributes, texp.exp_extra with
        | [], [] -> None
        | [], extra ->
          let rec loop = function
            | [] -> None
            | (_, _, attrs) :: rest ->
              (match find_attr attrs with
               | None -> loop rest
               | op -> op)
          in
          loop extra
        | attrs, _ -> find_attr attrs
      in
      try hole_payload >>= fun pl -> Some (id_of_payload pl) with
      | Invalid_payload pl ->
        raise_errorf
          ~loc:texp.exp_loc
          "(ppx_fillup) Illigal HOLE payload: %s"
          (string_of_payload pl)
    in
    let hole_id : string option option ref = ref get_id in
    let search_envvalues name path desc acc =
      let is_hole =
        match texp.exp_desc, find_attr overload_name desc.Types.val_attributes with
        | Texp_ident (_, { txt = Lident id; _ }, _), Some _ ->
          if name = id then Some (Some id) else None
        | _, _ -> None
      in
      match
        is_hole, instantiate None path desc, List.mem name (instantiate_deriving env)
      with
      | Some id, _, _ ->
        (*** 'texp' is HOLE ident ***)
        hole_id := Some id;
        acc
      | _, Some i, _ ->
        (*** Instantiate value in env ***)
        i :: acc
      | _, _, true ->
        (*** Instanceate 'deriving' functions ***)
        Mono { lpath = { level = 0; path }; desc; id = None } :: acc
      | _, _, _ -> acc
    in
    (*** Whether texp has [@HOLE id] ***)
    let instantiate_envvalue = Env.fold_values search_envvalues None env [] in
    match !hole_id with
    | None -> raise Not_hole
    | Some hid ->
      List.filter
        (fun (Mono i | Poly i) -> i.id = hid)
        (instantiate_mvalue @ instantiate_envvalue)

  (*** Match hole & INSTANCE list ***)
  let type_match (hole : T.expression) iset =
    let match_instance env hole = function
      | Poly { lpath; desc; id } ->
        (*** Whether INSTANCE is more general HOLE => match_type env hole instance ***)
        let rec loop path texp =
          match repr_type env texp with
          | Types.Tarrow (_, _, ret, _) ->
            if match_type env hole texp
            then Some { level = 0; path }
            else
              loop path ret >>= fun lpath -> Some { lpath with level = lpath.level + 1 }
          | _ -> if match_type env hole texp then Some { level = 0; path } else None
        in
        loop lpath.path desc.val_type >>= fun lpath -> Some (Poly { lpath; desc; id })
      | Mono { desc; _ } as inst ->
        (*** Whether HOLE is more general INSTANCE => match_type env instance hole ***)
        if match_type env desc.val_type hole
           (* || match_type env hole_texp desc.val_type *)
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
    loop iset

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

  let untyper f =
    let super = default_untyper in
    let self = { super with expr = f super } in
    self.structure self

  (*** Replace hole to instance ***)
  let replace_hole (super : Untypeast.mapper) self texp =
    (*** wether texp has INSTANCE ***)
    match make_iset texp with
    | exception Not_hole -> super.expr self texp
    | iset ->
      (*** HOLE & INSTANCE type-match ***)
      let loc, attrs = texp.exp_loc, texp.exp_attributes in
      (match type_match texp iset with
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
           texp.exp_type)

  (*** Fillup hole ***)
  let fillup str =
    Compmisc.init_path ();
    let env = Compmisc.initial_env () in
    let rec loop str =
      (* Format.eprintf "\n%a\n" Pprintast.structure str; *)
      let tstr = type_structure env str in
      let str' = untyper replace_hole tstr in
      if str = str' then str' else loop str'
    in
    loop str
end

open Ppxlib
open Ast_helper

class preprocess =
  object (this)
    inherit Ppxlib.Ast_traverse.map as super

    method id_binding ~loc name =
      Vb.mk
        ~loc
        (Pat.var
           ~loc
           ~attrs:[ Attr.mk ~loc (mkloc ~loc overload_name) (PStr []) ]
           { txt = name; loc })
        (mk_voidexpr ~loc ())

    method vbs_of_id_binding ~loc vbs =
      let collect_overload_id = function
        | None -> raise Not_id
        | Some id -> this#id_binding ~loc id
      in
      let mk_idset =
        let rec loop acc = function
          | [] -> acc
          | { pvb_pat; _ } :: rest ->
            let attrs = pvb_pat.ppat_attributes in
            (match
               find_attr' instance_name attrs, find_attr' instance_with_ctxt_name attrs
             with
             | None, None -> loop acc rest
             | Some p, _ | _, Some p -> loop (id_of_payload' p :: acc) rest)
        in
        loop [] vbs
      in
      List.fold_left
        (fun acc pl ->
          match collect_overload_id pl with
          | exception Not_id -> acc
          | vb -> vb :: acc)
        vbs
        (uniq @@ mk_idset)

    method! expression expr =
      let loc, attrs = expr.pexp_loc, expr.pexp_attributes in
      match expr.pexp_desc with
      (*** HOLE syntax : __ ***)
      | Pexp_ident { txt = Lident "__"; _ } -> mkhole ~loc ~attrs None
      (*** Generate 'id' binding ***)
      | Pexp_let (flag, vbs, e) ->
        super#expression
          { expr with pexp_desc = Pexp_let (flag, this#vbs_of_id_binding ~loc vbs, e) }
      (*** Generate 'id' binding when instantiate module ***)
      | Pexp_open ({ popen_expr; _ }, rest) ->
        (match
           find_attr' instance_name popen_expr.pmod_attributes >>= idopt_of_payload'
         with
         | None -> super#expression expr
         | Some id ->
           (match id with
            | None ->
              super#expression
              @@ expr_dummy_binding ~loc id { popen_expr with pmod_attributes = [] } rest
            | Some name ->
              let expr' =
                Exp.open_ ~loc
                @@ Opn.mk ~loc
                @@ Mod.structure
                     ~loc
                     [ stri_dummy_binding ~loc id { popen_expr with pmod_attributes = [] }
                     ; Str.value ~loc Nonrecursive [ this#id_binding ~loc name ]
                     ]
              in
              super#expression @@ expr' rest))
      | _ -> super#expression expr

    method! structure_item stri =
      let loc = stri.pstr_loc in
      match stri.pstr_desc with
      (*** Generate 'id' binding ***)
      | Pstr_value (flag, vbs) ->
        super#structure_item
          { stri with pstr_desc = Pstr_value (flag, this#vbs_of_id_binding ~loc vbs) }
      (*** Generate 'id' binding when instantiate module ***)
      | Pstr_open { popen_expr; _ } ->
        (match
           find_attr' instance_name popen_expr.pmod_attributes >>= idopt_of_payload'
         with
         | None -> super#structure_item stri
         | Some id ->
           let stri_binding =
             stri_dummy_binding ~loc id { popen_expr with pmod_attributes = [] }
           in
           (match id with
            | None -> super#structure_item stri_binding
            | Some name ->
              let stri' =
                Str.open_ ~loc
                @@ Opn.mk ~loc
                @@ Mod.structure
                     ~loc
                     [ stri_binding
                     ; Str.value ~loc Nonrecursive [ this#id_binding ~loc name ]
                     ]
              in
              super#structure_item stri'))
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
      | _ -> super#expression expr

    method! structure_item stri =
      let loc = stri.pstr_loc in
      match stri.pstr_desc with
      | Pstr_value (flag, vbs) ->
        (***  Remove id binding ***)
        let vbs' = this#remove_id_binding vbs in
        (match vbs' with
         | [] -> [%stri ()]
         | vbs' -> super#structure_item { stri with pstr_desc = Pstr_value (flag, vbs') })
      | _ -> super#structure_item stri
  end

(*** Transform structure ***)
let transform (str : Parsetree.structure) =
  if Ocaml_common.Ast_mapper.tool_name () = "ocamldoc"
     || Ocaml_common.Ast_mapper.tool_name () = "ocamldep"
  then (new preprocess)#structure str |> (new postprocess)#structure
  else (
    let str =
      (new preprocess)#structure str
      |> To_current_ocaml.structure
      (* |> expr_mapper Typed.alert_filled *)
      |> Typed.fillup
      |> Of_current_ocaml.structure
      |> (new postprocess)#structure
    in
    Format.eprintf "\n%a\n" Pprintast.structure str;
    str)
