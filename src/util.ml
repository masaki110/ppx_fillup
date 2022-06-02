let mk_dummy_module =
  let cnt = ref 0 in
  fun () ->
    cnt := !cnt + 1;
    "Fillup_dummy_module" ^ string_of_int !cnt

let print_out x =
  let out = open_out @@ "/tmp/fillup_out" in
  output_string out x;
  close_out out

let mkloc ~loc txt =
  let open Ppxlib in
  { txt; loc }

let mknoloc txt = mkloc ~loc:!Ast_helper.default_loc txt

let expr_mapper f str =
  let super = Ast_mapper.default_mapper in
  let self = { super with expr = f super } in
  self.structure self str

let untyp_expr_mapper f tstr =
  let super = Untypeast.default_mapper in
  let self = { super with expr = f super } in
  self.structure self tstr

let evar ~loc ~attrs ident =
  Ast_helper.Exp.ident ~loc ~attrs
    (mknoloc (Longident.Lident (Ident.name ident)))

open Parsetree

let mkhole =
  let cnt = ref 0 in
  fun ~loc ->
    cnt := !cnt + 1;
    let typ = Ast_helper.Typ.var @@ "fillup_hole" ^ string_of_int !cnt in
    [%expr (assert false : [%t typ]) [@HOLE]]

let mkattr name ~loc =
  { attr_name = mkloc ~loc name; attr_payload = PStr []; attr_loc = loc }

let attr_exists attrs txt =
  List.exists
    (fun (attr : Parsetree.attribute) -> attr.attr_name.txt = txt)
    attrs

let rmattrs attrs txt =
  List.filter
    (fun (attr : Parsetree.attribute) -> attr.attr_name.txt != txt)
    attrs
    