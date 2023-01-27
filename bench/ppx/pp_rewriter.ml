open Ppxlib

let idlist = ref []
let mdlist = ref []

class replace_pp =
  let match_string sub txt =
    let explode s =
      let rec expl i l = if i < 0 then l else expl (i - 1) (s.[i] :: l) in
      expl (String.length s - 1) []
    in
    let sub = explode sub in
    let rec loop cnt sub' txt =
      match (sub', txt) with
      | c1 :: rest1, c2 :: rest2 ->
          if c1 = c2 then loop cnt rest1 rest2 else loop cnt sub rest2
      | [], rest -> loop (cnt + 1) sub rest
      | _, [] -> cnt
    in
    loop 0 sub (explode txt)
  in
  object (this)
    inherit Ppxlib.Ast_traverse.map as super

    method! expression exp =
      let loc = exp.pexp_loc in
      match exp.pexp_desc with
      | Pexp_apply
          ( func,
            ( lbl1,
              ({ pexp_desc = Pexp_constant (Pconst_string (text, _, _)); _ } as
              arg1) )
            :: rest )
        when match_string "%a" text > 0 ->
          let addpp = function
            | Lident name ->
                if List.mem name !idlist then () else idlist := name :: !idlist
            | Ldot (lid, _) ->
                if List.mem lid !mdlist then () else mdlist := lid :: !mdlist
            | _ -> ()
          in
          let rec args acc = function
            | ( lbl2,
                ({ pexp_desc = Pexp_ident { txt = Lident "__"; _ }; _ } as arg2)
              )
              :: (lbl3, arg3)
              :: rest ->
                args ((lbl3, arg3) :: (lbl2, arg2) :: acc) rest
            | (lbl2, ({ pexp_desc = Pexp_ident { txt; _ }; _ } as arg2))
              :: (lbl3, arg3)
              :: rest ->
                addpp txt;
                args
                  ((lbl3, arg3)
                  :: ( lbl2,
                       {
                         arg2 with
                         pexp_desc =
                           Pexp_ident { txt = Lident "__"; loc = arg2.pexp_loc };
                       } )
                  :: acc)
                  rest
            | (lbl2, ({ pexp_desc = Pexp_constant _; _ } as arg2)) :: rest ->
                args ((lbl2, arg2) :: acc) rest
            | [] -> (lbl1, arg1) :: List.rev acc
            | l :: rest -> args (l::acc) rest
            (* function
               | ( lbl2,
                   ({ pexp_desc = Pexp_ident { txt = Lident "__"; _ }; _ } as arg2)
                 )
                 :: (lbl3, arg3)
                 :: rest ->
                   args ((lbl3, arg3) :: (lbl2, arg2) :: acc) rest
               | (lbl2, ({ pexp_desc = Pexp_ident { txt; _ }; _ } as arg2))
                 :: (lbl3, arg3)
                 :: rest ->
                   addpp txt;
                   args cntp cntpa
                     ((lbl3, arg3)
                     :: ( lbl2,
                          {
                            arg2 with
                            pexp_desc =
                              Pexp_ident { txt = Lident "__"; loc = arg2.pexp_loc };
                          } )
                     :: acc)
                     rest
               | [] -> (lbl1, arg1) :: List.rev acc
               | _ -> assert false *)
          in
          Ast_helper.Exp.apply ~loc ~attrs:exp.pexp_attributes
            (this#expression func) (args [] rest)
      | _ -> super#expression exp
  end

class inst_of_pp =
  object
    inherit Ppxlib.Ast_traverse.map as super

    method! structure_item stri =
      match stri.pstr_desc with
      | Pstr_value
          ( flag,
            ({
               pvb_pat = { ppat_desc = Ppat_var { txt = name; _ }; _ };
               pvb_attributes = attrs;
               pvb_loc = loc;
               _;
             } as vb)
            :: _ )
        when List.mem name !idlist
             && List.for_all
                  (fun attr -> attr.attr_name.txt != "instance")
                  attrs ->
          {
            stri with
            pstr_desc =
              Pstr_value
                ( flag,
                  [
                    {
                      vb with
                      pvb_attributes =
                        Ast_helper.Attr.mk { txt = "instance"; loc } (PStr [])
                        :: attrs;
                    };
                  ] );
          }
      | _ -> super#structure_item stri
  end

let transform (str : Parsetree.structure) =
  let str' = (new inst_of_pp)#structure @@ (new replace_pp)#structure str in
  let loc = Location.none in
  let rec open_str acc lids =
    match lids with
    | lid :: rest ->
        open_str
          (Ast_helper.Str.mk
             (Pstr_open
                {
                  popen_expr =
                    {
                      pmod_desc = Pmod_ident { txt = lid; loc };
                      pmod_loc = loc;
                      pmod_attributes = [];
                    };
                  popen_override = Fresh;
                  popen_loc = loc;
                  popen_attributes = [];
                })
          :: acc)
          rest
    | [] -> acc
  in
  let str'' =
    List.map
      (fun str ->
        Ast_helper.Str.mk
        @@ Pstr_extension (({ txt = "fillup"; loc }, PStr [ str ]), []))
      (open_str [] !mdlist)
  in
  str'' @ str'

let () = Driver.register_transformation ~impl:transform "pp_rewriter"
