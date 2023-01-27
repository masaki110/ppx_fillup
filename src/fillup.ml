open Util

module Typeful = struct
  open Parsetree

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

  let mark_alert exp =
    let expstr =
      Format.asprintf "ppx_fillup Filled: %a" Pprintast.expression exp
    in
    let _innerstr exp = Format.asprintf "%a" Pprintast.expression exp in
    let payload = Ast_helper.Exp.constant (Ast_helper.Const.string expstr) in
    let attr =
      {
        Parsetree.attr_name = { txt = "ppwarning"; loc = Location.none };
        attr_payload =
          PStr
            [ { pstr_desc = Pstr_eval (payload, []); pstr_loc = exp.pexp_loc } ];
        attr_loc = exp.pexp_loc;
      }
    in
    { exp with pexp_attributes = attr :: exp.Parsetree.pexp_attributes }

  let alert_filled (super : Ast_mapper.mapper) (self : Ast_mapper.mapper)
      (exp : Parsetree.expression) =
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

  let rec apply_holes n exp =
    if n = 0 then exp
    else
      let loc = exp.pexp_loc in
      apply_holes (n - 1)
      @@ to_exp [%expr [%e of_exp exp] [%e of_exp (mkhole' ~loc)]]

  let rec match_instance env hole path inst =
    let inst_tdesc : Types.type_desc = Compatibility.repr_type env inst in
    match inst_tdesc with
    | Tarrow (_, _, ret, _) -> (
        if
          (* prerr_endline (Path.name path); *)
          Compatibility.match_type env hole inst
        then (* prerr_endline "true\n"; *)
          Some (Mono path)
        else
          match match_instance env hole path ret with
          | Some (Mono p) -> Some (Multi (1, p))
          | Some (Multi (n, p)) -> Some (Multi (n + 1, p))
          | None -> None)
    | _ ->
        if Compatibility.match_type env hole inst then
          (* prerr_endline "true\n"; *)
          Some (Mono path)
        else (* prerr_endline "false\n"; *)
          None

  let make_instances env =
    let md_values env =
      let mds env =
        Env.fold_modules
          (fun name _ md acc ->
            if Str.(string_match (regexp "Dummy_module_fillup") name 0) then
              md :: acc
            else acc)
          None env []
      in
      let rec str_items path md =
        match md.Types.md_type with
        | Mty_signature sg ->
            List.fold_left
              (fun acc -> function
                | Types.Sig_value (ident, desc, _) -> begin
                    match path with
                    | None -> (Path.Pident ident, desc) :: acc
                    | Some p -> (Path.Pdot (p, Ident.name ident), desc) :: acc
                  end
                | _ -> acc)
              [] sg
        | Mty_alias p -> str_items (Some p) (Env.find_module p env)
        | Mty_functor _ | Mty_ident _ -> []
      in
      List.concat @@ List.map (str_items None) (mds env)
    in
    let env_values env =
      Env.fold_values
        (fun _ path desc acc ->
          if
            desc.val_attributes
            |> List.exists (fun attr ->
                   attr.attr_name.txt = "instance"
                   || attr.attr_name.txt = "inst")
          then
            match path with
            | Pdot (_, s) -> (Path.Pident (Ident.create_local s), desc) :: acc
            | _ -> (path, desc) :: acc
          else acc)
        None env []
    in
    env_values env @ md_values env

  let resolve_instances (texp : Typedtree.expression) =
    let rec find_instances = function
      | ((p, desc) : instance) :: rest -> (
          (* print_endline
             @@ Format.asprintf "%s : %a" (Path.name p) Printtyp.type_expr
                  desc.val_type; *)
          match match_instance texp.exp_env texp.exp_type p desc.val_type with
          | Some p -> (p, desc.val_type) :: find_instances rest
          | None -> find_instances rest)
      | [] -> []
    in
    find_instances (make_instances texp.exp_env)

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
    (* print_endline @@ Format.asprintf "%a" Printtyp.type_expr texp.exp_type; *)
    match resolve_instances texp with
    | [ (Mono p, _) ] -> evar' ~loc ~attrs p
    | [ (Multi (n, p), _) ] ->
        to_exp [%expr [%e of_exp @@ apply_holes n @@ evar' ~loc ~attrs p]]
    | _ :: _ as xs ->
        let rec show_path_list = function
          | (p, t) :: ps ->
              Format.asprintf "%s : %a" (show_path p) Printtyp.type_expr t
              ^ ",\n"
              ^ show_path_list ps
          | [] -> "nil"
        in
        Location.raise_errorf ~loc
          "(ppx_fillup) Instance overlapped: %a \n[ %s ]" Printtyp.type_expr
          texp.exp_type (show_path_list xs)
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
      let str' = expr_mapper alert_filled str' in
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

let replace_hashhash = (new Typeless.replace_hashhash_with_holes)#structure
let typer_untyper = Typeful.loop_typer_untyper
