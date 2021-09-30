[@@@ warnerror "-26"]
open Parsetree

let hole loc = 
  [%expr (assert false)[@HOLE]] 

class my_map = object
  inherit Ppxlib.Ast_traverse.map as super
  method! expression exp =
    match exp.pexp_desc with
    | Pexp_apply(
        {pexp_desc=Pexp_ident({txt=Lident("##"); _}); pexp_loc=loc_hole; _}, 
        [(_, arg1); (_, arg2)]
      ) -> 
        Ast_helper.Exp.apply ~loc:exp.pexp_loc ~attrs:exp.pexp_attributes 
          arg1 
          [(Nolabel, hole loc_hole); (Nolabel, arg2)]
    | _ -> super#expression exp
  end

let makeinstance env ty ident instty =
  if Ctype.matches env ty instty then
    Some ident
  else
    None

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
  | Env_value (s, ident, descr) ->
    (* print_tab lvl;
    print_endline @@ Ident.name ident; *)
    begin match makeinstance env ty ident descr.val_type with
    | Some i -> i :: find_instances lvl s
    | None -> find_instances lvl s
    end
  | Env_open (s, path) ->
      let str_items mdecl =
        match mdecl.Types.md_type with
        | Mty_signature sg -> sg
        | Mty_functor _ -> [] 
        | _ -> assert false
      in
      let lvl = lvl + 1 in
      let rest = find_instances lvl s in
      (* print_tab lvl;
      print_endline @@ "module: " ^ Path.name path; *)
      let md = Env.find_module path env in
      List.fold_left (fun res -> function
        | Types.Sig_value (ident, descr, _) ->
          (* print_tab lvl;
          print_endline @@ "  " ^ Ident.name ident; *)
          begin match makeinstance env ty ident descr.val_type with
          | Some i -> i :: res
          | _ -> res
          end
        | _ -> res)
        rest (str_items md)
  in
  find_instances 0 (Env.summary env)

let myuntyper = 
  let super = Untypeast.default_mapper in
  {Untypeast.default_mapper with
    expr = 
      (fun self (texp:Typedtree.expression) ->
        match texp.exp_attributes with
        | [{Parsetree.attr_name={txt="HOLE"; _}; attr_loc=loc; _}] ->
          (* prerr_endline "here!"; *)
          begin match resolve_instances texp.exp_type texp.exp_env with
          | ident::_ -> 
            Ast_helper.Exp.ident 
              (Location.mknoloc 
                (Longident.Lident (Ident.name ident)))
          | [] ->
            Location.raise_errorf ~loc "Instance not found:%a" 
              Printtyp.type_expr texp.exp_type
          end
        | _ -> super.expr self texp)
  }

let transform str =
  let str = (new my_map)#structure str in
  Compmisc.init_path (); 
  let env = Compmisc.initial_env () in
  let (tstr, _, _, _) = Typemod.type_structure env str in
  myuntyper.structure myuntyper tstr

let () =
  Ppxlib.Driver.register_transformation
    ~impl:transform
    "ppx_fillup"
