open Parsetree

let name_dummy_module = "Fillup_dummy_module"
let name_hole_type = "fillup_hole"
let print_ident id = print_endline @@ Ident.name id

let print_out f x =
  let out = open_out "/tmp/fillup_out.ml" in
  output_string out (Format.asprintf "%a" f x);
  close_out out

let prexp e = print_out Pprintast.expression e

let mkloc ~loc txt =
  let open Ppxlib in
  { txt; loc }

let mknoloc txt = mkloc ~loc:!Ast_helper.default_loc txt

module Fillup_hole = struct
  type id = Poly of int * Ident.t | Mono of Ident.t
  type instance = Ident.t * Types.value_description

  let make_hole =
    let cnt = ref 0 in
    fun ~loc ->
      cnt := !cnt + 1;
      let typ = Ast_helper.Typ.var @@ name_hole_type ^ string_of_int !cnt in
      [%expr (assert false : [%t typ]) [@HOLE]]

  let mark_alert loc exp : Parsetree.expression =
    let expstr =
      Format.asprintf "ppx_fillup Filled: %a" Pprintast.expression exp
    in
    let payload : Parsetree.expression =
      Ast_helper.Exp.constant (Ast_helper.Const.string expstr)
    in
    let attr =
      {
        Parsetree.attr_name = { txt = "ppwarning"; loc = Location.none };
        attr_payload =
          PStr [ { pstr_desc = Pstr_eval (payload, []); pstr_loc = loc } ];
        attr_loc = Location.none;
      }
    in
    { exp with pexp_attributes = attr :: exp.Parsetree.pexp_attributes }

  let rec apply_holes n exp =
    if n = 0 then exp
    else
      let loc = exp.pexp_loc in
      apply_holes (n - 1) [%expr [%e exp] [%e make_hole ~loc]]

  let evar ident =
    Ast_helper.Exp.ident (mknoloc (Longident.Lident (Ident.name ident)))

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

  let is_instance (desc : Types.value_description) =
    List.exists
      (fun attr -> attr.attr_name.txt = "instance")
      desc.val_attributes

  let make_instances env =
    let ident_of_path =
      Path.(
        function
        | Pident id -> id
        | Pdot (_, s) -> Ident.create_local s
        | _ -> assert false)
    in
    (* let rec search_sg acc = function
         | [] -> acc
         | x :: xs -> (
             match x with
             | Types.Sig_value (ident, desc, _) ->
                 if
                   is_instance desc
                   (* && Str.string_match
                        (Str.regexp @@ name_dummy_module ^ "[0-9]+")
                        (Ident.name ident) 0 *)
                 then search_sg ((ident, desc) :: acc) xs
                 else search_sg acc xs
             | _ -> search_sg acc xs)
       in *)
    Env.fold_values
      (fun _ path desc acc ->
        if is_instance desc then (ident_of_path path, desc) :: acc else acc)
      None env []
  (* @ Env.fold_modules
      (fun _ _ decl acc ->
        match decl.Types.md_type with
        | Mty_signature sg -> search_sg acc sg
        | _ -> acc)
      None env [] *)

  let resolve_instances (texp : Typedtree.expression) =
    let rec find_instances = function
      | ((id, desc) : instance) :: rest -> (
          match match_instance texp.exp_env texp.exp_type id desc.val_type with
          | Some i -> i :: find_instances rest
          | None -> find_instances rest)
      | [] -> []
    in
    let instances = make_instances texp.exp_env in
    find_instances instances

  let fillup_hole self (super : Untypeast.mapper) attr
      (texp : Typedtree.expression) =
    match attr with
    | { Parsetree.attr_name = { txt = "HOLE"; _ }; attr_loc = loc; _ } ->
        mark_alert loc
          (match resolve_instances texp with
          | [ Mono ident ] -> evar ident
          | [ Poly (n, ident) ] -> [%expr [%e apply_holes n @@ evar ident]]
          | _ :: _ ->
              Location.raise_errorf ~loc
                "ppx_fillup Error : Instance overlapped : %a" Printtyp.type_expr
                texp.exp_type
          | [] ->
              Location.raise_errorf ~loc
                "ppx_fillup Error : Instance not found : %a" Printtyp.type_expr
                texp.exp_type)
    | _ -> super.expr self texp

  let untyper =
    let super = Untypeast.default_mapper in
    {
      Untypeast.default_mapper with
      expr =
        (fun self (texp : Typedtree.expression) ->
          match (texp.exp_attributes, texp.exp_extra) with
          | attr :: _, _ -> fillup_hole self super attr texp
          | _, (_, _, attr :: _) :: _ -> fillup_hole super self attr texp
          | _ -> super.expr self texp);
    }

  let rec loop_typer_untyper str =
    Compmisc.init_path ();
    let env = Compmisc.initial_env () in
    let tstr, _, _, _ = Typemod.type_structure env str in
    let untypstr = untyper.structure untyper tstr in
    if str = untypstr then (
      print_out Pprintast.structure untypstr;
      untypstr)
    else loop_typer_untyper untypstr

  class replace_hashhash_with_holes =
    object (this)
      inherit Ppxlib.Ast_traverse.map as super

      method! expression exp =
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
              (this#expression arg1)
              [ (Nolabel, make_hole ~loc); (Nolabel, this#expression arg2) ]
        | _ -> super#expression exp
    end

  let transform str =
    let str = (new replace_hashhash_with_holes)#structure str in
    loop_typer_untyper str
end

module Driver = struct
  open Ppxlib

  (* Declaration of extension [%HOLE] 
   * ref : https://tarides.com/blog/2019-05-09-an-introduction-to-ocaml-ppx-ecosystem *)
  let hole =
    Extension.declare "HOLE" Extension.Context.expression
      Ast_pattern.(pstr nil)
      (fun ~loc ~path:_ -> Fillup_hole.make_hole ~loc)

  let counter =
    let cnt = ref 0 in
    cnt := !cnt + 1;
    string_of_int !cnt

  (* open%fillup M --> module Dummy = M;; open Dummy*)
  let open_instance_toplevel =
    Extension.declare "fillup" Extension.Context.structure_item
      Ast_pattern.(pstr @@ pstr_open __ ^:: nil)
      (fun ~loc ~path:_ open_module ->
        let mod_exp = open_module.popen_expr in
        let dummy_name = name_dummy_module ^ counter in
        {
          pstr_desc =
            Pstr_module
              {
                pmb_name = mkloc ~loc @@ Some dummy_name;
                pmb_expr = mod_exp;
                pmb_attributes = [];
                pmb_loc = loc;
              };
          pstr_loc = loc;
        })

  (* let open%fillup M in e --> let module Dummy = M in e  *)
  let open_instance_local =
    Extension.declare "fillup" Extension.Context.expression
      Ast_pattern.(pstr @@ pstr_eval (pexp_open __ __) nil ^:: nil)
      (fun ~loc ~path:_ open_module expr ->
        let mod_exp = open_module.popen_expr in
        let dummy_name = name_dummy_module ^ counter in
        {
          pexp_desc =
            Pexp_letmodule (mkloc ~loc @@ Some dummy_name, mod_exp, expr);
          pexp_loc = loc;
          pexp_loc_stack = [];
          pexp_attributes = [];
        })

  let driver =
    Driver.register_transformation
      ~extensions:[ hole; open_instance_toplevel; open_instance_local ]
      ~instrument:(Driver.Instrument.make ~position:After Fillup_hole.transform)
      "ppx_fillup"
end

let () = Driver.driver