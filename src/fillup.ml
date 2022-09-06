open Parsetree
open Util

type id = Poly of int * Path.t | Mono of Path.t
type instance = Path.t * Types.value_description

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
  match check_attr_expr exp "FILLED" with
  | None -> super.expr self exp
  | Some e -> mark_alert e

let rec apply_holes n exp =
  if n = 0 then exp
  else
    let loc = exp.pexp_loc in
    apply_holes (n - 1) [%expr [%e exp] [%e mkhole ~loc]]

let rec match_instance env holety ident instty =
  let instty = Ctype.repr @@ Ctype.expand_head env instty in
  match instty.desc with
  | Tarrow (_, _, ret, _) -> (
      if Ctype.matches env holety instty then Some (Mono ident)
      else
        match match_instance env holety ident ret with
        | Some (Mono ident) -> Some (Poly (1, ident))
        | Some (Poly (n, ident)) -> Some (Poly (n + 1, ident))
        | None -> None)
  | _ -> if Ctype.matches env holety instty then Some (Mono ident) else None

let make_instances env =
  let md_values env =
    let mds env =
      Env.fold_modules
        (fun name _ md acc ->
          if Str.(string_match (regexp "Fillup_dummy_module") name 0) then
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
        if attr_exists desc.val_attributes "instance" then
          match path with
          | Pdot (_, s) -> (Path.Pident (Ident.create_local s), desc) :: acc
          | _ -> (path, desc) :: acc
        else acc)
      None env []
  in
  env_values env @ md_values env

let resolve_instances (texp : Typedtree.expression) =
  let rec find_instances = function
    | ((id, desc) : instance) :: rest -> (
        match match_instance texp.exp_env texp.exp_type id desc.val_type with
        | Some i -> i :: find_instances rest
        | None -> find_instances rest)
    | [] -> []
  in
  find_instances (make_instances texp.exp_env)

let fillup_hole (texp : Typedtree.expression) =
  let loc = texp.exp_loc in
  let attrs = mkattr "FILLED" ~loc :: texp.exp_attributes in
  match resolve_instances texp with
  | [ Mono path ] -> evar' ~loc ~attrs path
  | [ Poly (n, path) ] -> [%expr [%e apply_holes n @@ evar' ~loc ~attrs path]]
  | _ :: _ ->
      Location.raise_errorf ~loc "ppx_fillup Error : Instance overlapped %a"
        Printtyp.type_expr texp.exp_type
  | [] ->
      Location.raise_errorf ~loc "ppx_fillup Error : Instance not found %a"
        Printtyp.type_expr texp.exp_type

let search_hole (super : Untypeast.mapper) (self : Untypeast.mapper)
    (texp : Typedtree.expression) =
  match check_attr_texpr texp "HOLE" with
  | None -> super.expr self texp
  | Some texp -> fillup_hole texp

let rec loop_typer_untyper =
  fun str ->
    Compmisc.init_path ();
    let env = Compmisc.initial_env () in
    let tstr, _, _, _ = Typemod.type_structure env str in
    let str' = untyp_expr_mapper search_hole tstr in
    if str = str' then
      let str' = expr_mapper alert_filled str' in
      (* print_out
         @@ Format.asprintf "%a" Pprintast.structure str'
         ^ "\n\ntyping & untyping loop : "
         ^ string_of_int !cnt; *)
      str'
    else loop_typer_untyper str'

let replace_hashhash_with_holes (super : Ast_mapper.mapper)
    (self : Ast_mapper.mapper) exp =
  match exp.pexp_desc with
  | Pexp_apply
      ( {
          pexp_desc = Pexp_ident { txt = Lident "##"; _ };
          pexp_loc = loc_hole;
          _;
        },
        [ (_, arg1); (_, arg2) ] ) ->
      let loc = loc_hole in
      Ast_helper.Exp.apply ~loc:exp.pexp_loc ~attrs:exp.pexp_attributes
        (self.expr self arg1)
        [ (Nolabel, mkhole ~loc); (Nolabel, self.expr self arg2) ]
  | _ -> super.expr self exp

let transform str =
  let str = expr_mapper replace_hashhash_with_holes str in
  loop_typer_untyper str
