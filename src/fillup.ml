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

  let get_iset ((texp_hole : Typedtree.expression), payload) =
    let env = texp_hole.exp_env in
    let equal_stri inst_stri hole_stri =
      let err_hole () =
        Location.raise_errorf ~loc:hole_stri.pstr_loc
          "(ppx_fillup) Illigal HOLE payload: %a" Pprintast.structure_item
          hole_stri
      in
      let err_instance () =
        Location.raise_errorf ~loc:inst_stri.pstr_loc
          "(ppx_fillup) Illigal Instance payload: %a" Pprintast.structure_item
          inst_stri
      in
      match (inst_stri.pstr_desc, hole_stri.pstr_desc) with
      | Pstr_eval (inst_exp, _), Pstr_eval (hole_exp, _) -> begin
          match (inst_exp.pexp_desc, hole_exp.pexp_desc) with
          | Pexp_ident inst_lidloc, Pexp_ident hole_lidloc ->
              inst_lidloc.txt = hole_lidloc.txt
          | _, Pexp_ident _ -> err_instance ()
          | Pexp_ident _, _ -> err_hole ()
          | _ -> err_instance ()
        end
      | _, Pstr_eval _ -> err_hole ()
      | Pstr_eval _, _ -> err_instance ()
      | _ -> err_instance ()
    in
    let md_vals env =
      let dummy_md env =
        Env.fold_modules
          (fun name _ md acc ->
            if Str.(string_match (regexp "Dummy_module_fillup") name 0) then
              md :: acc
            else acc)
          None env []
      in
      let resolve_dummy_md md =
        let rec search_sig path md =
          match md.Types.md_type with
          | Mty_signature sg ->
              List.fold_left
                (fun acc -> function
                  | Types.Sig_value (ident, desc, _) ->
                      let p = Path.Pdot (path, Ident.name ident) in
                      if
                        desc.val_attributes
                        |> List.exists (fun attr ->
                               attr.attr_name.txt = "instance_with_context")
                      then
                        Poly
                          ({ level = 0; current_path = p }, Env.find_value p env)
                        :: acc
                      else
                        (* get type from the env (not using signature, as it abbreviates the path)  *)
                        Mono (p, Env.find_value p env) :: acc
                  | _ -> acc)
                [] sg
          | Mty_alias p -> search_sig p (Env.find_module p env)
          | _ -> []
        in
        match md.Types.md_type with
        | Mty_alias p -> search_sig p (Env.find_module p env)
        | _ -> []
      in
      List.concat @@ List.map resolve_dummy_md (dummy_md env)
    in
    let env_vals env =
      let deriving_types =
        Env.fold_types
          (fun name _ decl acc ->
            if
              decl.type_attributes
              |> List.exists (fun attr -> attr.attr_name.txt = "deriving")
            then name :: acc
            else acc)
          None env []
      in
      (* let rec f acc = function
           | [] -> prerr_endline acc
           | stri :: rest -> (
               match stri.pstr_desc with
               | Pstr_eval (e, _) -> begin
                   match e.pexp_desc with
                   | Pexp_ident _ -> f ("id, " ^ acc) rest
                   | Pexp_tuple (exp :: _) -> begin
                       match exp.pexp_desc with
                       | Pexp_ident _ -> f ("tp, " ^ acc) rest
                       | _ -> f ("other2" ^ acc) rest
                     end
                   | _ -> f ("other2, " ^ acc) rest
                 end
               | _ -> f ("other1, " ^ acc) rest)
         in *)
      let match_attrs inst_name attrs =
        let rec loop = function
          | [] -> false
          | { attr_name; attr_payload; _ } :: _attrs
            when attr_name.txt = inst_name -> (
              match (attr_payload, payload) with
              | PStr [], PStr [] -> true
              | PStr [ stri1 ], PStr [ stri2 ] -> equal_stri stri1 stri2
              | _ -> false)
          | _ :: attrs -> loop attrs
        in
        loop attrs
      in
      Types.(
        let match_instance name path desc acc =
          if match_attrs "instance" desc.val_attributes then
            Mono (path, desc) :: acc
          else if match_attrs "instance_with_context" desc.val_attributes then
            Poly ({ level = 0; current_path = path }, desc) :: acc
          else if
            List.exists
              (fun type_name ->
                Str.(
                  string_match
                    (regexp ("\\(show\\|pp\\|equal\\|compare\\)_" ^ type_name))
                    name 0))
              deriving_types
          then Mono (path, desc) :: acc
          else acc
        in
        Env.fold_values match_instance None env [])
    in
    env_vals env @ md_vals env

  let check_instance hole =
    Typedtree.(
      let texp_hole = fst hole in
      let match_instance env hole inst =
        match inst with
        | Poly (lp, desc) -> begin
            let rec loop path texp =
              let inst_desc : Types.type_desc =
                Compatibility.repr_type env texp
              in
              match inst_desc with
              | Tarrow (_, _, ret, _) -> (
                  if Compatibility.match_type env hole texp then
                    Some { level = 0; current_path = path }
                  else
                    match loop path ret with
                    | Some inst -> Some { inst with level = inst.level + 1 }
                    | None -> None)
              | _ ->
                  if Compatibility.match_type env hole texp then
                    Some { level = 0; current_path = path }
                  else None
            in
            match loop lp.current_path desc.val_type with
            | Some lp -> Some (Poly (lp, desc))
            | None -> None
          end
        | Mono (_, desc) ->
            if Compatibility.match_type env hole desc.val_type then Some inst
            else None
      in
      let rec loop = function
        | (inst : instance) :: rest -> (
            match match_instance texp_hole.exp_env texp_hole.exp_type inst with
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
                let texp = { texp with exp_attributes = acc @ rest } in
                Some (texp, attr.attr_payload)
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

  let replace_instance (super : Untypeast.mapper) (self : Untypeast.mapper)
      (texp : Typedtree.expression) =
    match texp_is_hole texp with
    | None -> super.expr self texp
    | Some ((texp, _) as hole) -> (
        let loc, attrs = (texp.exp_loc, texp.exp_attributes) in
        match check_instance hole with
        | [ Mono (p, _) ] -> evar ~loc ~attrs p
        | [ Poly (lp, _) ] ->
            apply_holes lp.level @@ evar ~loc ~attrs lp.current_path
        | _ :: _ as l ->
            Location.raise_errorf ~loc
              "(ppx_fillup) Instance overlapped: %a \n[ %s ]" Printtyp.type_expr
              texp.exp_type (show_instances l)
        | [] ->
            Location.raise_errorf ~loc "(ppx_fillup) Instance not found: %a"
              Printtyp.type_expr texp.exp_type)

  let fillup str =
    Compmisc.init_path ();
    let env = Compmisc.initial_env () in
    let rec loop str =
      let tstr = Compatibility.type_structure env str in
      let str' = untyp_expr_mapper replace_instance tstr in
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
        (* | Pexp_apply
            ( { pexp_desc = Pexp_ident { txt = Lident "##"; _ }; _ },
              [ (_, arg1); arg2 ] ) ->
            this#expression
            @@ Exp.apply arg1 [ (Nolabel, Exp.apply hole [ arg2 ]) ] *)
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
