type path = Multi of int * Path.t | Mono of Path.t
type instance = Path.t * Types.value_description

let mk_dummy_module_name =
  let cnt = ref 0 in
  fun () ->
    cnt := !cnt + 1;
    "Fillup_dummy_module" ^ string_of_int !cnt

(* let print_out x =
  let out = open_out @@ "/tmp/fillup_out" in
  output_string out x;
  close_out out *)

let mkloc ~loc txt =
  let open Ppxlib in
  { txt; loc }

let mknoloc txt = mkloc ~loc:!Ast_helper.default_loc txt

(* let attr_exists attrs txt =
  List.exists
    (fun (attr : Parsetree.attribute) -> attr.attr_name.txt = txt)
    attrs *)

let open_instnce_attr_name = "open_instnce"

let open_instnce_attr =
  Parsetree.(
    fun ~loc ?(payload = PStr []) () ->
      {
        attr_name = mkloc ~loc open_instnce_attr_name;
        attr_payload = payload;
        attr_loc = loc;
      })

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

let lid_of_path path =
  let rec loop =
    Path.(
      function
      | Pident id -> Longident.Lident (Ident.name id)
      | Pdot (p, str) -> Longident.Ldot (loop p, str)
      | Papply (p1, p2) -> Longident.Lapply (loop p1, loop p2))
  in
  loop path

let evar' ~loc ~attrs path =
  Ast_helper.Exp.ident ~loc ~attrs @@ mknoloc @@ lid_of_path path

open Ppxlib

let mkhole =
  let cnt = ref 0 in
  fun ~loc ->
    cnt := !cnt + 1;
    let typ = Ast_helper.Typ.var @@ "fillup_hole" ^ string_of_int !cnt in
    [%expr (assert false : [%t typ]) [@HOLE]]

let mkhole' ~loc = Selected_ast.To_ocaml.copy_expression @@ mkhole ~loc

let match_attrs attrs txt =
  let rec loop acc = function
    | [] -> acc
    | attr :: attrs ->
        if attr.attr_name.txt = txt then acc @ attrs
        else loop (attr :: acc) attrs
  in
  loop [] attrs
