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
    let env = hole_texp.exp_env in
    let search_mdvals md =
      Types.(
        let rec find_instance path md =
          let check_sig path acc = function
            | Sig_value (ident, sig_desc, _) ->
                let name = Ident.name ident in
                let path = Path.Pdot (path, name) in
                collect_inst ~exc:Include env name path sig_desc acc
            | _ -> acc
          in
          match md.md_type with
          | Mty_signature sg -> List.fold_left (check_sig path) [] sg
          | Mty_alias p -> find_instance p (Env.find_module p env)
          | _ -> []
        in
        match md.md_type with
        | Mty_alias p -> find_instance p (Env.find_module p env)
        | _ -> [])
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
      List.concat @@ List.map search_mdvals (dummy_md env)
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
    match hole_cls with
    (* | Some v when List.mem v [ "+"; "-"; "*"; "/" ] -> check_class [] *)
    | _ -> check_class (search_envvals env @ resolve_dummy_md env)

  let check_instance hole =
    Typedtree.(
      let match_instance env hole ctx_inst =
        match ctx_inst with
        | Poly { lpath; desc; cls } -> begin
            let rec loop path texp =
              let inst_desc : Types.type_desc =
                Compatibility.repr_type env texp
              in
              match inst_desc with
              | Tarrow (_, _, ret, _) -> (
                  if Compatibility.match_type env hole texp then
                    Some { level = 0; path }
                  else
                    match loop path ret with
                    | Some inst -> Some { inst with level = inst.level + 1 }
                    | None -> None)
              | _ ->
                  if Compatibility.match_type env hole texp then
                    Some { level = 0; path }
                  else None
            in
            match loop lpath.path desc.val_type with
            | Some lpath -> Some (Poly { lpath; desc; cls })
            | None -> None
          end
        | Mono { desc; _ } ->
            if Compatibility.match_type env hole desc.val_type then
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
      loop (get_iset hole))

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
      | Some ({ hole_texp; _ } as hole) -> (
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
              Location.raise_errorf ~loc "(ppx_fillup) Instance not found: %a"
                Printtyp.type_expr hole_texp.exp_type))

  let fillup str =
    Compmisc.init_path ();
    let env = Compmisc.initial_env () in
    (*** add 'open%fillup Pkg.Op' ***)
    let str =
      Ast_helper.(
        (Str.module_
        @@ Mb.mk
             (mkloc (Some (mk_dummy_md_name ())))
             (Mod.ident
             @@ mkloc
             @@ Longident.Ldot (Longident.Lident "Pkg", "Op")))
        :: str)
    in
    let rec loop str =
      let tstr = Compatibility.type_structure env str in
      let str' = untyp_expr_mapper instance_replace_hole tstr in
      if str = str' then
        (* (function | [] -> () | x::_ -> Location.print_loc "%a" x.pstr_loc) *)
        str'
      else loop str'
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
            @@ Exp.apply ~loc ~attrs func [ (Nolabel, Exp.apply hole args) ]
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
            this#expression
            @@ Exp.apply ~loc ~attrs func ((Nolabel, hole) :: args)
        (*** Fillup label arguments ***)
        | Pexp_apply
            ( func,
              ( _,
                {
                  pexp_desc =
                    Pexp_apply
                      ( { pexp_desc = Pexp_ident { txt = Lident "~!"; _ }; _ },
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
            @@ Exp.apply ~loc ~attrs func ((Labelled name, hole) :: args)
        (*** Arithmetic ***)
        | Pexp_apply
            ( ({ pexp_desc = Pexp_ident { txt = Lident arith; _ }; _ } as exp),
              args )
          when List.mem arith [ "+"; "-"; "*"; "/" ] ->
            this#expression
            @@ Exp.apply ~loc ~attrs
                 (mkhole ~loc ~attrs ~payload:(PStr [ Str.eval exp ]) ())
                 args
        | _ -> super#expression exp
    end
end

(* let alert_mapper = expr_mapper Typeful.alert_filled *)
let fillup = Typeful.fillup
let preprocess = (new Typeless.preprocess)#structure
