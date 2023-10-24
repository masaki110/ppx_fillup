open Util

module Typeful = struct
  open Parsetree

  (* let alert_filled (super : Ast_mapper.mapper) self (exp : Parsetree.expression)
       =
     let mark_alert exp =
       let expstr =
         Format.asprintf "ppx_fillup Filled: %a" Pprintast.expression exp
       in
       let payload = Ast_helper.Exp.constant (Ast_helper.Const.string expstr) in
       let attr =
         {
           Parsetree.attr_name = { txt = "ppwarning"; loc = Location.none };
           attr_payload =
             PStr
               [
                 { pstr_desc = Pstr_eval (payload, []); pstr_loc = exp.pexp_loc };
               ];
           attr_loc = exp.pexp_loc;
         }
       in
       { exp with pexp_attributes = attr :: exp.Parsetree.pexp_attributes }
     in
     let check_attr_expr exp txt =
       let rec loop acc = function
         | [] -> None
         | attr :: attrs ->
             if attr.attr_name.txt = txt then
               Some { exp with pexp_attributes = acc @ attrs }
             else loop (attr :: acc) attrs
       in
       loop [] exp.pexp_attributes
     in
     match check_attr_expr exp "Filled" with
     | None -> super.expr self exp
     | Some e -> mark_alert e *)

  let rec apply_holes n (exp : expression) =
    if n = 0 then exp
    else
      let loc, attrs = (exp.pexp_loc, exp.pexp_attributes) in
      apply_holes (n - 1)
        {
          exp with
          pexp_desc = Pexp_apply (exp, [ (Nolabel, mkhole ~loc ~attrs ()) ]);
        }

  let get_iset { hole_texp; hole_cls } =
    let find_instance env path =
      let rec loop path =
        let md = Env.find_module path env in
        let check_sig path acc = function
          | Types.Sig_value (ident, sig_desc, _) ->
              let name = Ident.name ident in
              collect_inst ~exc:Include env name
                (Path.Pdot (path, name))
                sig_desc acc
          | _ -> acc
        in
        match md.md_type with
        | Mty_signature sg -> List.fold_left (check_sig path) [] sg
        | Mty_alias p -> loop p
        | _ -> []
      in
      loop path
    in
    let search_mdvals env md =
      Types.(
        match md.md_type with Mty_alias p -> find_instance env p | _ -> [])
    in
    let resolve_dummy_md env =
      let dummy_md env =
        Env.fold_modules
          (fun name _ md acc ->
            if Str.(string_match (regexp "Dummy_module_fillup") name 0) then
              md :: acc
            else acc)
          None env []
      in
      List.(concat @@ map (search_mdvals env) (dummy_md env))
    in
    let search_envvals env =
      Env.fold_values (collect_inst ~exc:Exclude env) None env []
    in
    let check_class l =
      let rec loop acc = function
        | [] -> acc
        | (Mono { cls; _ } as inst) :: vs | (Poly { cls; _ } as inst) :: vs ->
            if cls = hole_cls then loop (inst :: acc) vs else loop acc vs
      in
      loop [] l
    in
    let env = hole_texp.exp_env in
    let _arith_path = Path.Pident (Ident.create_persistent "Ppx_fillup") in
    match hole_cls with
    | Some txt when is_arith txt -> check_class (find_instance env _arith_path)
    | _ -> check_class (search_envvals env @ resolve_dummy_md env)

  let check_instance hole =
    let match_instance env hole_texp ctx_inst =
      match ctx_inst with
      | Poly { lpath; desc; cls } -> begin
          let rec loop path texp =
            let inst_desc = Compatibility.repr_type env texp in
            match inst_desc with
            | Types.Tarrow (_, _, ret, _) -> (
                if Compatibility.match_type env hole_texp texp then
                  Some { level = 0; path }
                else
                  match loop path ret with
                  | Some inst -> Some { inst with level = inst.level + 1 }
                  | None -> None)
            | _ ->
                if Compatibility.match_type env hole_texp texp then
                  Some { level = 0; path }
                else None
          in
          match loop lpath.path desc.val_type with
          | Some lpath -> Some (Poly { lpath; desc; cls })
          | None -> None
        end
      | Mono { desc; _ } ->
          if Compatibility.match_type env hole_texp desc.val_type then
            Some ctx_inst
          else None
    in
    let rec loop = function
      | inst :: rest -> (
          match
            match_instance hole.hole_texp.exp_env hole.hole_texp.exp_type inst
          with
          | Some i -> i :: loop rest
          | None -> loop rest)
      | [] -> []
    in
    loop (get_iset hole)

  let texp_is_hole texp =
    Typedtree.(
      let match_attrs texp =
        let rec attr_is_hole acc = function
          | [] -> None
          | attr :: rest ->
              if attr.attr_name.txt = "HOLE" then
                let hole_texp = { texp with exp_attributes = acc @ rest } in
                let hole_cls =
                  try get_class attr
                  with Invalid_payload ->
                    Location.raise_errorf ~loc:attr.attr_loc
                      "(ppx_fillup) Illigal HOLE payload: %a" Pprintast.payload
                      attr.attr_payload
                in
                Some { hole_texp; hole_cls }
              else attr_is_hole (attr :: acc) rest
        in
        match (texp.exp_attributes, texp.exp_extra) with
        | [], [] -> None
        | [], extra ->
            let rec loop_extra = function
              | [] -> None
              | (_, _, attrs) :: rest ->
                  let res = attr_is_hole [] attrs in
                  if res = None then loop_extra rest else res
            in
            loop_extra extra
        | attrs, _ -> attr_is_hole [] attrs
      in
      match_attrs texp)

  let instance_replace_hole (super : Untypeast.mapper) (self : Untypeast.mapper)
      texp =
    Typedtree.(
      match texp_is_hole texp with
      | None -> super.expr self texp
      | Some ({ hole_texp; hole_cls } as hole) -> (
          let loc, attrs = (hole_texp.exp_loc, hole_texp.exp_attributes) in
          match check_instance hole with
          | [ Mono { lpath; _ } ] -> evar ~loc ~attrs lpath.path
          | [ Poly { lpath; _ } ] ->
              apply_holes lpath.level @@ evar ~loc ~attrs lpath.path
          | _ :: _ as l ->
              Location.raise_errorf ~loc
                "(ppx_fillup) Instance overlapped: %a \n[ %s ]"
                Printtyp.type_expr hole_texp.exp_type (show_instances l)
          | [] ->
              let arith_cls =
                try which_arith hole_cls
                with Not_Arithmetic_Operator ->
                  Location.raise_errorf ~loc
                    "(ppx_fillup) Instance not found: %a" Printtyp.type_expr
                    hole_texp.exp_type
              in
              Ast_helper.Exp.ident ~loc ~attrs
              @@ mknoloc
              @@ Longident.Ldot (Longident.Lident "Ppx_fillup", arith_cls)))

  let fillup str =
    Compmisc.init_path ();
    let env = Compmisc.initial_env () in
    let rec loop str =
      let tstr = Compatibility.type_structure env str in
      let str' = untyp_expr_mapper instance_replace_hole tstr in
      if str = str' then str' else loop str'
    in
    loop str
end

module Typeless = struct
  (* open Ppxlib *)

  class preprocess =
    object (this)
      inherit Ppxlib.Ast_traverse.map as super

      method! expression exp =
        let open Ast_helper in
        let loc, attrs = (exp.pexp_loc, exp.pexp_attributes) in
        let hole =
          Cast.of_ocaml_exp
          @@ mkhole ~loc ~attrs:(Cast.to_ocaml_exp exp).pexp_attributes ()
        in
        match exp.pexp_desc with
        (*** HOLE syntax ***)
        | Pexp_ident { txt = Lident "__"; _ } -> hole
        (*** Fillup type cast ***)
        | Pexp_apply
            ( {
                pexp_desc =
                  Pexp_apply
                    ( { pexp_desc = Pexp_ident { txt = Lident "!!"; _ }; _ },
                      [ (_, func) ] );
                pexp_attributes;
                _;
              },
              args ) ->
            let attrs = attrs @ pexp_attributes in
            this#expression
            @@ Exp.apply ~attrs func [ (Nolabel, Exp.apply hole args) ]
        (*** Fillup any expr ***)
        | Pexp_apply
            ( {
                pexp_desc =
                  Pexp_apply
                    ( { pexp_desc = Pexp_ident { txt = Lident "??"; _ }; _ },
                      [ (_, func) ] );
                pexp_attributes;
                _;
              },
              args ) ->
            let attrs = attrs @ pexp_attributes in
            this#expression @@ Exp.apply ~attrs func ((Nolabel, hole) :: args)
        (*** Fillup label arguments ***)
        | Pexp_apply
            ( func,
              ( _,
                {
                  pexp_desc =
                    Pexp_apply
                      ( {
                          pexp_desc = Pexp_ident { txt = Lident "~!"; _ };
                          pexp_attributes;
                          _;
                        },
                        [
                          ( _,
                            {
                              pexp_desc = Pexp_ident { txt = Lident name; _ };
                              _;
                            } );
                        ] );
                  _;
                } )
              :: args ) ->
            this#expression
            @@ Exp.apply ~attrs:pexp_attributes func
                 ((Labelled name, hole) :: args)
        (*** Arithmetic ***)
        | Pexp_apply
            ( ({
                 pexp_desc = Pexp_ident { txt = Lident name; _ };
                 pexp_attributes;
                 _;
               } as exp),
              args )
          when is_arith name ->
            this#expression
            @@ Exp.apply ~attrs:pexp_attributes
                 (mkhole ~payload:(PStr [ Str.eval exp ]) ())
                 args
        | _ -> super#expression exp
    end
end

let transform (str : Parsetree.structure) =
  (* let alert_mapper = expr_mapper Typeful.alert_filled in *)
  let preprocess = (new Typeless.preprocess)#structure in
  Ppxlib.(
    if
      Ocaml_common.Ast_mapper.tool_name () = "ocamldoc"
      || Ocaml_common.Ast_mapper.tool_name () = "ocamldep"
    then preprocess str
    else
      preprocess str
      |> Selected_ast.To_ocaml.copy_structure
      (* |> alert_mapper *)
      |> Typeful.fillup
      |> Selected_ast.Of_ocaml.copy_structure)