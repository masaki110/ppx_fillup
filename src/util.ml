module Cast = struct
  open Ppxlib

  let to_ocaml_exp = Selected_ast.To_ocaml.copy_expression
  let of_ocaml_exp = Selected_ast.Of_ocaml.copy_expression

  let to_ocaml_typ = Selected_ast.To_ocaml.copy_core_type
  let of_ocaml_typ = Selected_ast.Of_ocaml.copy_core_type
end


(* type path = Multi of int * Path.t | Mono of Path.t *)
type lpath = { level : int; current_path : Path.t }

type instance =
  | Mono of (Path.t * Types.value_description)
  | Poly of (lpath * Types.value_description)

let show_instances l =
  let show_instance inst =
    let lpath_name p = Path.name p.current_path in
    match inst with
    | Mono (p, desc) ->
        Format.asprintf "%s : %a" (Path.name p) Printtyp.type_expr desc.val_type
    | Poly (p, desc) ->
        Format.asprintf "%s : %a" (lpath_name p) Printtyp.type_expr
          desc.val_type
  in
  let rec loop acc = function
    | [] -> "[]"
    | [ i ] -> acc ^ show_instance i ^ " ]"
    | i :: rest -> loop (acc ^ show_instance i ^ ",\n  ") rest
  in
  loop "[ " l

let mk_dummy_md_name =
  let cnt = ref 0 in
  fun () ->
    cnt := !cnt + 1;
    "Dummy_module_fillup" ^ string_of_int !cnt

(* let print_out x =
   let out = open_out @@ "/tmp/fillup_out" in
   output_string out x;
   close_out out *)

let mkloc ~loc txt = Location.{ txt; loc }
let mknoloc txt = mkloc ~loc:Location.none txt

(* let attr_exists attrs txt =
   List.exists
     (fun (attr : Parsetree.attribute) -> attr.attr_name.txt = txt)
     attrs *)

let open_instnce_attr_name = "open_instance"

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

(* let evar ~loc ~attrs ident =
   Ast_helper.Exp.ident ~loc ~attrs
     (mknoloc (Longident.Lident (Ident.name ident))) *)

let lid_of_path path =
  let rec loop =
    Path.(
      function
      | Pident id -> Longident.Lident (Ident.name id)
      | Pdot (p, str) -> Longident.Ldot (loop p, str)
      | Papply (p1, p2) -> Longident.Lapply (loop p1, loop p2))
  in
  loop path

let evar ~loc ~attrs path =
  Ast_helper.Exp.ident ~loc ~attrs @@ mknoloc @@ lid_of_path path


let mkhole =
  let cnt = ref 0 in
  fun ~loc ?(attrs = []) ?(str = []) () ->
    cnt := !cnt + 1;
    let open Ast_helper in
    {
      (Cast.to_ocaml_exp ([%expr (assert false : [%t Cast.of_ocaml_typ @@ Typ.var @@ "fillup_hole" ^ string_of_int !cnt])])) with
      pexp_attributes = Attr.mk ~loc { txt = "HOLE"; loc } (PStr str) :: attrs;
    }

let mkhole' ~loc ?(attrs=[]) () = Cast.of_ocaml_exp @@ mkhole ~loc ~attrs ()


let match_attrs attrs txt =
  let rec loop acc = function
    | [] -> acc
    | (attr:Parsetree.attribute) :: attrs ->
        if attr.attr_name.txt = txt then acc @ attrs
        else loop (attr :: acc) attrs
  in
  loop [] attrs
