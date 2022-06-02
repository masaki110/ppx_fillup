open Parsetree
open Util

type id = Poly of int * Ident.t | Mono of Ident.t
type instance = Ident.t * Types.value_description

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
          [ { pstr_desc = Pstr_eval (payload, []); pstr_loc = exp.pexp_loc } ];
      attr_loc = Location.none;
    }
  in
  { exp with pexp_attributes = attr :: exp.Parsetree.pexp_attributes }

let alert_filled (super : Ast_mapper.mapper) (self : Ast_mapper.mapper)
    (exp : Parsetree.expression) =
  if attr_exists exp.pexp_attributes "FILLED" then
    let exp = super.expr self exp in
    mark_alert
      { exp with pexp_attributes = rmattrs exp.pexp_attributes "FILLED" }
  else super.expr self exp

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
  let ident_of_path =
    Path.(
      function
      | Pident id -> id
      | Pdot (_, s) -> Ident.create_local s
      | _ -> assert false)
  in
  Env.fold_values
    (fun _ path desc acc ->
      if attr_exists desc.val_attributes "instance" then
        (ident_of_path path, desc) :: acc
      else acc)
    None env []

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
  | [ Mono ident ] -> evar ~loc ~attrs ident
  | [ Poly (n, ident) ] -> [%expr [%e apply_holes n @@ evar ~loc ~attrs ident]]
  | _ :: _ ->
      Location.raise_errorf ~loc "ppx_fillup Error : Instance overlapped %a"
        Printtyp.type_expr texp.exp_type
  | [] ->
      Location.raise_errorf ~loc "ppx_fillup Error : Instance not found %a"
        Printtyp.type_expr texp.exp_type

let is_hole (texp : Typedtree.expression) =
  let rec search_texp_extra = function
    | (_, _, attrs) :: rest ->
        if attr_exists attrs "HOLE" then true else search_texp_extra rest
    | [] -> false
  in
  if attr_exists texp.exp_attributes "HOLE" then true
  else search_texp_extra texp.exp_extra

let search_hole (super : Untypeast.mapper) (self : Untypeast.mapper)
    (texp : Typedtree.expression) =
  if is_hole texp then
    fillup_hole
      { texp with exp_attributes = rmattrs texp.exp_attributes "HOLE" }
  else super.expr self texp

let rec loop_typer_untyper str =
  Compmisc.init_path ();
  let env = Compmisc.initial_env () in
  let tstr, _, _, _ = Typemod.type_structure env str in
  let str' = untyp_expr_mapper search_hole tstr in
  if str = str' then
    (* print_out (Format.asprintf "%a" Pprintast.structure str'); *)
    expr_mapper alert_filled str'
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
