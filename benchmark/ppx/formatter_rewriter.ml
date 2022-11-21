open Ppxlib

class replace_pp =
  let pp = ref "" in
  object (this)
    inherit Ppxlib.Ast_traverse.map as super

    method! expression exp =
      match exp.pexp_desc with
      | Pexp_apply
          ( func,
            [
              (lbl1, arg1);
              ( lbl2,
                ({
                   pexp_desc =
                     Pexp_ident { txt = Ldot (Lident "Pprintast", _); _ };
                   pexp_loc = loc;
                   _;
                 } as arg2) );
              (lbl3, arg3);
            ] ) ->
          Ast_helper.Exp.apply ~loc:exp.pexp_loc ~attrs:exp.pexp_attributes
            (this#expression func)
            [
              (lbl1, arg1);
              ( lbl2,
                { arg2 with pexp_desc = Pexp_ident { txt = Lident "__"; loc } }
              );
              (lbl3, arg3);
            ]
      | Pexp_apply
          ( func,
            [
              ( lbl1,
                ({
                   pexp_desc = Pexp_constant (Pconst_string (txt, _, _));
                   pexp_loc = loc;
                   _;
                 } as arg1) );
              (lbl2, arg2);
              (lbl3, arg3);
            ] )
        when (try Str.(search_forward (regexp "%a") txt 0)
              with Not_found -> -1)
             >= 0 ->
          pp := txt;
          Ast_helper.Exp.apply ~loc:exp.pexp_loc ~attrs:exp.pexp_attributes
            (this#expression func)
            [
              (lbl1, arg1);
              ( lbl2,
                { arg2 with pexp_desc = Pexp_ident { txt = Lident "__"; loc } }
              );
              (lbl3, arg3);
            ]
      | _ -> super#expression exp

    method! structure_item stri =
      match stri.pstr_desc with
      | Pstr_value
          ( flag,
            [
              ({
                 pvb_pat =
                   { ppat_desc = Ppat_constant (Pconst_string (txt, _, _)); _ };
                 pvb_attributes = attrs;
                 pvb_loc = loc;
                 _;
               } as vb);
            ] )
        when txt = !pp ->
          {
            stri with
            pstr_desc =
              Pstr_value
                ( flag,
                  [
                    {
                      vb with
                      pvb_attributes =
                        {
                          attr_name = { txt = !pp; loc };
                          attr_payload = PStr [];
                          attr_loc = loc;
                        }
                        :: attrs;
                    };
                  ] );
          }
      | _ -> super#structure_item stri
  end

let add_open ~loc md_name =
  {
    pstr_desc =
      Pstr_extension
        ( ( { txt = "fillup"; loc },
            PStr
              [
                {
                  pstr_desc =
                    Pstr_open
                      {
                        popen_expr =
                          {
                            pmod_desc = Pmod_ident { txt = Lident md_name; loc };
                            pmod_loc = loc;
                            pmod_attributes = [];
                          };
                        popen_override = Fresh;
                        popen_loc = loc;
                        popen_attributes = [];
                      };
                  pstr_loc = loc;
                };
              ] ),
          [] );
    pstr_loc = Location.none;
  }

let transform (str : Parsetree.structure) =
  match str with
  | stri :: _ ->
      let open_inst = add_open ~loc:Location.none "Pprintast" in
      if stri != open_inst then open_inst :: (new replace_pp)#structure str
      else (new replace_pp)#structure str
  | [] -> []

let () = Driver.register_transformation ~impl:transform "ppx_fillup"
