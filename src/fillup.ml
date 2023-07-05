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

  let texp_is_hole texp =
    Typedtree.(
      let match_attrs texp =
        let rec loop_attrs = function
          | [] -> None
          | attr :: rest ->
              if attr.attr_name.txt = "HOLE" then Some (texp, attr.attr_payload)
              else loop_attrs rest
        in
        match (texp.exp_attributes, texp.exp_extra) with
        | [], [] -> None
        | (_ :: _ as attrs), _ -> loop_attrs attrs
        | _, (_ :: _ as extra) ->
            let rec loop_extra = function
              | [] -> None
              | (_, _, attrs) :: rest ->
                  let tmp = loop_attrs attrs in
                  if tmp = None then loop_extra rest else tmp
            in
            loop_extra extra
      in
      match_attrs texp)

  let rec apply_holes n (exp : expression) =
    if n = 0 then exp
    else
      let loc, attrs = (exp.pexp_loc, exp.pexp_attributes) in
      apply_holes (n - 1)
        {
          exp with
          pexp_desc = Pexp_apply (exp, [ (Nolabel, mkhole ~loc ~attrs ()) ]);
        }

  let mk_iset ?(hole_payload = PStr []) env =
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
      let hole_str = (function PStr s -> s | _ -> []) hole_payload in
      let rec instance_with_payload inst_name = function
        | [] -> false
        | { attr_name; attr_payload = PStr str; _ } :: attrs ->
            (* if attr_name.txt = inst_name then
               Format.printf "%a = %a : %b\n" Pprintast.structure str
                 Pprintast.structure hole_str (str == hole_str); *)
            (attr_name.txt = inst_name && str = hole_str)
            || instance_with_payload inst_name attrs
        | _ -> assert false
      in
      Env.fold_values
        (fun name path desc acc ->
          if instance_with_payload "instance" desc.val_attributes then
            Mono (path, desc) :: acc
          else if
            instance_with_payload "instance_with_context" desc.val_attributes
          then Poly ({ level = 0; current_path = path }, desc) :: acc
          else if
            List.exists
              (fun type_name ->
                Str.(
                  string_match
                    (regexp ("\\(show\\|pp\\|equal\\|compare\\)_" ^ type_name))
                    name 0))
              deriving_types
          then Mono (path, desc) :: acc
          else acc)
        None env []
    in
    env_vals env @ md_vals env

  let check_instance (hole : Typedtree.expression) ~hole_payload =
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
          match match_instance hole.exp_env hole.exp_type inst with
          | Some i -> i :: loop rest
          | None -> loop rest)
      | [] -> []
    in
    loop (mk_iset hole.exp_env ~hole_payload)

  let replace_instance (super : Untypeast.mapper) (self : Untypeast.mapper)
      (texp : Typedtree.expression) =
    match texp_is_hole texp with
    | None -> super.expr self texp
    | Some (hole, hole_payload) -> (
        let loc = hole.exp_loc in
        let attrs = hole.exp_attributes in
        match check_instance hole ~hole_payload with
        | [ Mono (p, _) ] -> evar ~loc ~attrs p
        | [ Poly (lp, _) ] ->
            apply_holes lp.level @@ evar ~loc ~attrs lp.current_path
        | _ :: _ as l ->
            Location.raise_errorf ~loc
              "(ppx_fillup) Instance overlapped: %a \n[ %s ]" Printtyp.type_expr
              hole.exp_type (show_instances l)
        | [] ->
            Location.raise_errorf ~loc "(ppx_fillup) Instance not found: %a"
              Printtyp.type_expr hole.exp_type)

  (* let replace_instance' (super : Untypeast.mapper) (self : Untypeast.mapper)
       (texp : Typedtree.expression) =
     match texp_is_hole texp with
     | None -> super.expr self texp
     | Some (hole, hole_payload) -> (
         let loc = hole.exp_loc in
         let attrs = hole.exp_attributes in
         match check_instance hole ~hole_payload with
         | [ Mono (p, _) ] -> evar ~loc ~attrs p
         | [ Poly (lp, _) ] ->
             apply_holes lp.level @@ evar ~loc ~attrs lp.current_path
         | _ -> super.expr self texp) *)

  let fillup str =
    (* let cnt = ref 0 in *)
    Compmisc.init_path ();
    let env = Compmisc.initial_env () in
    let rec loop str =
      (* cnt := !cnt + 1; *)
      (* print_endline (string_of_int !cnt); *)
      let tstr = Compatibility.type_structure env str in
      let str' = untyp_expr_mapper replace_instance tstr in
      if str = str' then
        (* let tstr = Compatibility.type_structure env str in
             untyp_expr_mapper replace_instance tstr *)
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
        let _print_expr (exp : Parsetree.expression) =
          match exp.pexp_desc with
          | Pexp_ident _ -> Format.eprintf "id  %a\n" Pprintast.expression exp
          | Pexp_apply _ -> Format.eprintf "app %a\n" Pprintast.expression exp
          | _ -> ()
        in
        let open Ast_helper in
        let loc = exp.pexp_loc in
        let attrs = exp.pexp_attributes in
        let hole =
          Cast.of_ocaml_exp
          @@ mkhole ~loc ~attrs:(Cast.to_ocaml_exp exp).pexp_attributes ()
        in
        match exp.pexp_desc with
        (* HOLE syntax *)
        | Pexp_ident { txt = Lident "__"; _ } -> hole
        (* Fillup type cast *)
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
                      [ (_, arg) ] );
                pexp_attributes;
                _;
              },
              args ) ->
            let attrs = attrs @ pexp_attributes in
            this#expression
            @@ Exp.apply ~loc ~attrs arg [ (Nolabel, Exp.apply hole args) ]
        (* Fillup label arguments *)
        (* | Pexp_apply
            ( { pexp_desc = Pexp_ident { txt = Lident "!!"; _ }; _ },
              (_, arg1)
              :: (_, { pexp_desc = Pexp_ident { txt = Lident name; _ }; _ })
              :: args ) ->
            this#expression
            @@ Exp.apply ~loc ~attrs arg1 ((Labelled name, hole) :: args) *)
        | Pexp_apply
            ( func,
              ( _,
                {
                  pexp_desc =
                    Pexp_apply
                      ( { pexp_desc = Pexp_ident { txt = Lident "!!"; _ }; _ },
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
            (* _print_expr arg; *)
            this#expression
            @@ Exp.apply ~loc ~attrs func ((Labelled name, hole) :: args)
        (* | Pexp_apply
             ( ({ pexp_desc = Pexp_ident { txt = Lident arith; loc }; _ } as
               _exp'),
               args )
           when List.mem arith [ "+"; "-"; "*"; "/" ] ->
             this#expression
             @@ Exp.apply ~loc ~attrs
                  (mkhole ~loc ~attrs
                     ~payload:
                       (PStr
                          [
                            Str.eval
                            @@ Exp.constant (Pconst_string (arith, loc, None));
                          ])
                     ())
                  args *)
        | _ -> super#expression exp
    end
end

let alert_mapper = expr_mapper Typeful.alert_filled
let fillup = Typeful.fillup
let preprocess = (new Typeless.preprocess)#structure
