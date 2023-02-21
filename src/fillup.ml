open Util

module Typeful = struct
  open Parsetree

  let alert_filled (super : Ast_mapper.mapper) self (exp : Parsetree.expression)
      =
    let mark_alert exp =
      let expstr =
        Format.asprintf "ppx_fillup Filled: %a" Pprintast.expression exp
      in
      (* let _innerstr exp = Format.asprintf "%a" Pprintast.expression exp in *)
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
    | Some e -> mark_alert e

  let check_attr_texpr texp txt =
    Typedtree.(
      let rec match_attrs acc texp = function
        | [] -> None
        | attr :: attrs ->
            if attr.attr_name.txt = txt then
              Some { texp with exp_attributes = acc @ attrs }
            else match_attrs (attr :: acc) texp attrs
      in
      let rec match_extra acc texp = function
        | [] -> None
        | (ex, loc, attrs) :: rest ->
            let rec loop acc' = function
              | [] -> match_extra ((ex, loc, attrs) :: acc) texp rest
              | attr' :: attrs' ->
                  if attr'.attr_name.txt = txt then
                    Some
                      {
                        texp with
                        exp_extra = ((ex, loc, acc' @ attrs') :: acc) @ rest;
                      }
                  else loop (attr' :: acc') attrs
            in
            loop [] attrs
      in
      match match_attrs [] texp texp.exp_attributes with
      | Some e -> Some e
      | None -> match_extra [] texp texp.exp_extra)

  let rec apply_holes n exp =
    if n = 0 then exp
    else
      let loc = exp.pexp_loc in
      apply_holes (n - 1)
      @@ to_exp [%expr [%e of_exp exp] [%e of_exp (mkhole' ~loc)]]

  let match_instance env hole inst =
    match inst with
    | Poly (lp, desc) -> begin
        let rec loop path texp =
          let inst_desc : Types.type_desc = Compatibility.repr_type env texp in
          match inst_desc with
          | Tarrow (_, _, ret, _) -> (
              if
                (* prerr_endline (Path.name path); *)
                Compatibility.match_type env hole texp
              then Some { level = 0; current_path = path }
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
    | Mono (_p, desc) ->
        (* prerr_endline @@ Format.asprintf "%s" (Path.name _p); *)
        if Compatibility.match_type env hole desc.val_type then Some inst
        else None

  let make_instances env =
    let md_values env =
      let dummy_md env =
        Env.fold_modules
          (fun name _ md acc ->
            if Str.(string_match (regexp "Dummy_module_fillup") name 0) then
              md :: acc
            else acc)
          None env []
      in
      let resolve_dummy_md md =
        let rec search_sg path expop md =
          match expop with
          | None -> begin
              match md.Types.md_type with
              | Mty_signature sg ->
                  List.fold_left
                    (fun acc -> function
                      | Types.Sig_value (ident, desc, _) ->
                          Mono (Path.Pdot (path, Ident.name ident), desc) :: acc
                      | _ -> acc)
                    [] sg
              | Mty_alias p -> search_sg p None (Env.find_module p env)
              | _ -> []
            end
          | Some name -> begin
              match md.Types.md_type with
              | Mty_signature sg ->
                  List.fold_left
                    (fun acc -> function
                      | Types.Sig_value (ident, desc, _)
                        when Ident.name ident = name ->
                          Mono (Path.Pdot (path, Ident.name ident), desc) :: acc
                      | _ -> acc)
                    [] sg
              | Mty_alias p -> search_sg p (Some name) (Env.find_module p env)
              | _ -> []
            end
        in
        match md.Types.md_type with
        | Mty_alias p -> search_sg p None (Env.find_module p env)
        | _ -> []
      in
      List.concat @@ List.map resolve_dummy_md (dummy_md env)
    in
    let env_values env =
      Env.fold_values
        (fun _ path desc acc ->
          if
            desc.val_attributes
            |> List.exists (fun attr -> attr.attr_name.txt = "instance")
          then Mono (path, desc) :: acc
          else if
            desc.val_attributes
            |> List.exists (fun attr ->
                   attr.attr_name.txt = "instance_with_context")
          then Poly ({ level = 0; current_path = path }, desc) :: acc
          else acc)
        None env []
    in
    env_values env @ md_values env

  let resolve_instances (texp : Typedtree.expression) =
    let insts = make_instances texp.exp_env in
    let rec loop = function
      | (inst : instance) :: rest -> (
          match match_instance texp.exp_env texp.exp_type inst with
          | Some i -> i :: loop rest
          | None -> loop rest)
      | [] -> []
    in
    (* prerr_endline @@ show_instances insts; *)
    loop insts

  let mkattr name ~loc =
    { attr_name = mkloc ~loc name; attr_payload = PStr []; attr_loc = loc }

  let fillup_hole (texp : Typedtree.expression) =
    let loc = texp.exp_loc in
    let attrs =
      {
        attr_name = mkloc ~loc "Filled";
        attr_payload = PStr [];
        attr_loc = loc;
      }
      :: texp.exp_attributes
    in
    match resolve_instances texp with
    | [ Mono (p, _) ] -> evar' ~loc ~attrs p
    | [ Poly (lp, _) ] ->
        to_exp
          [%expr
            [%e
              of_exp
              @@ apply_holes lp.level
              @@ evar' ~loc ~attrs lp.current_path]]
    | _ :: _ as l ->
        Location.raise_errorf ~loc
          "(ppx_fillup) Instance overlapped: %a \n[ %s ]" Printtyp.type_expr
          texp.exp_type (show_instances l)
    | [] ->
        Location.raise_errorf ~loc "(ppx_fillup) Instance not found: %a"
          Printtyp.type_expr texp.exp_type

  let search_hole (super : Untypeast.mapper) (self : Untypeast.mapper)
      (texp : Typedtree.expression) =
    match check_attr_texpr texp "HOLE" with
    | None -> super.expr self texp
    | Some texp -> fillup_hole texp

  let rec loop_typer_untyper str =
    Compmisc.init_path ();
    let env = Compmisc.initial_env () in
    let tstr = Compatibility.type_structure env str in
    let str' = untyp_expr_mapper search_hole tstr in
    if str = str' then
      (* prerr_endline @@ Pprintast.string_of_structure str'; *)
      str'
    else loop_typer_untyper str'
end

module Typeless = struct
  open Ppxlib

  class replace_hashhash_with_holes =
    object (this)
      inherit Ppxlib.Ast_traverse.map as super

      method! expression exp =
        match exp.pexp_desc with
        | Pexp_ident { txt = Lident "__"; loc } -> mkhole ~loc
        | Pexp_apply
            ( {
                pexp_desc = Pexp_ident { txt = Lident "##"; _ };
                pexp_loc = loc;
                _;
              },
              [ (_, arg1); (_, arg2) ] ) ->
            Ast_helper.Exp.apply ~loc:exp.pexp_loc ~attrs:exp.pexp_attributes
              (this#expression arg1)
              [ (Nolabel, mkhole ~loc); (Nolabel, this#expression arg2) ]
        | _ -> super#expression exp
    end
end

let alert_mapper = expr_mapper Typeful.alert_filled
let typer_untyper = Typeful.loop_typer_untyper
let replace_hashhash = (new Typeless.replace_hashhash_with_holes)#structure
