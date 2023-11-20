open Parsetree

(* let default_loc = !Ast_helper.default_loc *)
(* let mkloc ~loc txt = Location.{ txt; loc } *)
let mkloc ?(loc = !Ast_helper.default_loc) x = Location.mkloc x loc
let mknoloc = Location.mknoloc

let expr_mapper f str =
  let super = Ast_mapper.default_mapper in
  let self = { super with expr = f super } in
  self.structure self str

(* let untyper = Untypeast.default_mapper *)
let untyper f tstr =
  let super = MyUntypeast.default_mapper in
  let self = { super with expr = f super } in
  self.structure self tstr

let lident_of_path = Compatibility.lident_of_path

let evar ~loc ~attrs path =
  Ast_helper.Exp.ident ~loc ~attrs @@ mknoloc @@ lident_of_path path

(* ***************************************************** *)
let hole_name = "HOLE"
let is_arith txt = List.mem txt [ "+"; "-"; "*"; "/" ]

exception Not_Arithmetic_Operator

let which_arith = function
  | Some "+" -> "addii"
  | Some "-" -> "subii"
  | Some "*" -> "mulii"
  | Some "/" -> "divii"
  | _ -> raise Not_Arithmetic_Operator

let arith_path = Path.Pident (Ident.create_persistent "Ppx_fillup")
let dummy_md_name = "FillupDummyMd"

let mk_dummy_md_name =
  let cnt = ref 0 in
  fun () ->
    cnt := !cnt + 1;
    dummy_md_name ^ string_of_int !cnt

let show_payload = function
  | PStr str -> Pprintast.string_of_structure str
  | _ -> ""

module Cast = struct
  open Ppxlib

  let to_exp = Selected_ast.To_ocaml.copy_expression
  let to_str = Selected_ast.To_ocaml.copy_structure
  let of_exp = Selected_ast.Of_ocaml.copy_expression
  let of_str = Selected_ast.Of_ocaml.copy_structure
end

(* let mkhole =
   Ast_helper.(
     let cnt = ref 0 in
     fun ?(loc = !default_loc) ?(attrs = []) ?(payload = PStr []) () ->
       cnt := !cnt + 1;
       {
         (Cast.to_exp
            [%expr
              (assert false
                : [%t
                    Ppxlib.Ast_helper.Typ.var
                    @@ "fillup_hole"
                    ^ string_of_int !cnt])])
         with
         pexp_attributes = Attr.mk ~loc { txt = "HOLE"; loc } payload :: attrs;
       }) *)
let mkhole =
  Ast_helper.(
    let cnt = ref 0 in
    fun ?(loc = !default_loc) ?(attrs = []) ?(payload = PStr []) () ->
      cnt := !cnt + 1;
      {
        (Cast.to_exp
           [%expr
             (Ppx_fillup.hole
               : [%t Ppxlib.Ast_helper.Typ.var @@ "hole" ^ string_of_int !cnt])])
        with
        pexp_attributes = Attr.mk ~loc { txt = "HOLE"; loc } payload :: attrs;
      })
(* let mkhole =
   Ast_helper.(
     fun ?(loc = !default_loc) ?(attrs = []) ?(payload = PStr []) () ->
       {
         (Cast.to_exp [%expr Ppx_fillup.hole]) with
         pexp_attributes = Attr.mk ~loc { txt = "HOLE"; loc } payload :: attrs;
       }) *)

let mkhole' ?(loc = !Ast_helper.default_loc) ?(attrs = []) ?(payload = PStr [])
    () =
  Cast.of_exp @@ mkhole ~loc ~attrs ~payload ()
