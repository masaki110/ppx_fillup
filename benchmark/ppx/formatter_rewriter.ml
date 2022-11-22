open Ppxlib

let ppastflag = ref true
let pplist = ref []

class replace_pp =
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
          ppastflag := false;
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
                ({ pexp_desc = Pexp_constant (Pconst_string (text, _, _)); _ }
                as arg1) );
              ( lbl2,
                ({ pexp_desc = Pexp_ident { txt = id; _ }; pexp_loc = loc; _ }
                as arg2) );
              (lbl3, arg3);
            ] )
        when (try Str.(search_forward (regexp "%a") text 0)
              with Not_found -> -1)
             >= 0
             && Longident.name id != "__" ->
          pplist := Longident.name id :: !pplist;
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
                 pvb_pat = { ppat_desc = Ppat_var { txt = name; _ }; _ };
                 pvb_attributes = attrs;
                 pvb_loc = loc;
                 _;
               } as vb);
            ] )
        when List.mem name !pplist ->
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
                          attr_name = { txt = "instance"; loc };
                          attr_payload = PStr [];
                          attr_loc = loc;
                        }
                        :: attrs;
                    };
                  ] );
          }
      | _ -> super#structure_item stri
  end

let open_fillup ~loc md_name =
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
    pstr_loc = loc;
  }

let transform (str : Parsetree.structure) =
  let open_inst = open_fillup ~loc:Location.none "Pprintast" in
  let str' = (new replace_pp)#structure @@ (new replace_pp)#structure str in
  let str' =
    if List.mem open_inst str || !ppastflag then str' else open_inst :: str'
  in
  str'

let () = Driver.register_transformation ~impl:transform "ppx_fillup"
