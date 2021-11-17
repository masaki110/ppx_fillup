[@@@ warnerror "-26"]
open Parsetree

let make_hole =
  let cnt = ref 0 in
  fun loc ->
    cnt := !cnt+1;
    let typ = Ast_helper.Typ.var @@ "fillup_tmp_" ^ (string_of_int !cnt) in
    [%expr ((assert false:[%t typ]))[@HOLE]]

let rec apply_holes n exp =
  if n=0 then
    exp
  else
    let loc=exp.pexp_loc in
    apply_holes (n-1) [%expr [%e exp] [%e make_hole loc]]

let evar ident = Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident (Ident.name ident)))

type instance_polymorphism = Poly of int * Ident.t | Nonpoly of Ident.t

let rec match_instance env holety ident instty =
  match (Ctype.repr @@ Ctype.expand_head env instty).desc with
  | Tarrow (_,_,ret,_) -> 
    if Ctype.matches env holety instty then
      Some (Nonpoly ident)
    else
      begin match match_instance env holety ident ret with
      | Some (Nonpoly ident) -> Some (Poly (1,ident))
      | Some (Poly (n,ident)) -> Some (Poly (n+1,ident))
      | None -> None
    end
  | _ -> 
    if Ctype.matches env holety instty then
      Some (Nonpoly ident)
    else  
      None

let is_instance (desc : Types.value_description) = 
  List.exists (fun attr -> attr.attr_name.txt="instance") desc.val_attributes 

let resolve_instances ty env =
  let rec find_instances lvl = function
  | Env.Env_empty -> []
  | Env_extension (s, _, _)
  | Env_modtype (s, _, _)
  | Env_class (s, _, _)
  | Env_cltype (s, _, _)
  | Env_functor_arg (s, _)
  | Env_type (s, _, _)
  | Env_constraints (s, _) 
  | Env_module (s, _, _, _) 
  | Env_copy_types s
  | Env_persistent (s, _)
  | Env_value_unbound (s, _, _)
  | Env_module_unbound (s, _, _) -> 
    find_instances lvl s
  | Env_value (s, ident, desc) ->
    if is_instance desc then
      begin match match_instance env ty ident desc.val_type with
      | Some i -> i :: find_instances lvl s
      | None -> find_instances lvl s
      end
    else find_instances lvl s
  | Env_open (s, path) ->
    let str_items mdecl =
      match mdecl.Types.md_type with
      | Mty_signature sg -> sg
      | Mty_functor _ -> [] 
      | _ -> assert false
    in
    let lvl = lvl + 1 in
    let rest = find_instances lvl s in
    let md = Env.find_module path env in
    List.fold_left (fun res -> function
      | Types.Sig_value (ident, desc, _) ->
        if is_instance desc then
          begin match match_instance env ty ident desc.val_type with
          | Some i -> i :: res
          | _ -> res
          end
        else find_instances lvl s
      | _ -> res)
        rest (str_items md)
  in
  find_instances 0 (Env.summary env)

let lookup_hole self (super:Untypeast.mapper) attr (texp:Typedtree.expression) = 
  match attr with
  | {Parsetree.attr_name={txt="HOLE"; _}; attr_loc=loc; _} ->
    begin match resolve_instances texp.exp_type texp.exp_env with
    | Nonpoly ident::_ -> evar ident 
    | Poly (n,ident)::_ -> [%expr [%e apply_holes n @@ evar ident]]
    | _ ->
      Location.raise_errorf ~loc "Instance not found:%a" Printtyp.type_expr texp.exp_type
    end
  | _ -> super.expr self texp

let untyper = 
  let super = Untypeast.default_mapper in
  {Untypeast.default_mapper with
    expr = fun self (texp:Typedtree.expression) ->
      match texp.exp_attributes, texp.exp_extra with
      | attr::_, _ -> lookup_hole self super attr texp
      | _, (_,_,attr::_)::_ -> lookup_hole super self attr texp
      | _ -> super.expr self texp
  }

let rec loop_typer_untyper str =
  Compmisc.init_path (); 
  let env = Compmisc.initial_env () in
  let (tstr, _, _, _) = Typemod.type_structure env str in
  let untypstr = untyper.structure untyper tstr in
  if str=untypstr then
    let out = open_out "/tmp/foo.ml" in
    output_string out (Format.asprintf "%a" Ocaml_common.Pprintast.structure untypstr);
    close_out out;
    untypstr
  else
    loop_typer_untyper untypstr

class replace_hashhash = object (this)
  inherit Ppxlib.Ast_traverse.map as super
  method! expression exp =
    match exp.pexp_desc with
    | Pexp_apply({pexp_desc=Pexp_ident({txt=Lident("##"); _}); pexp_loc=loc_hole; _}, [(_, arg1); (_, arg2)]) -> 
      let loc=loc_hole in
      Ast_helper.Exp.apply ~loc:exp.pexp_loc ~attrs:exp.pexp_attributes 
        (this#expression arg1)
        [(Nolabel, make_hole loc); (Nolabel, (this#expression arg2))]
    | _ -> super#expression exp
end

let transform str =
  let str = (new replace_hashhash)#structure str in
  loop_typer_untyper str

let () =
  Ppxlib.Driver.register_transformation
    ~instrument:(Ppxlib.Driver.Instrument.make ~position:After (transform))
    "ppx_fillup"