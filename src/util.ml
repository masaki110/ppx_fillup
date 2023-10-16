module Cast = struct
  open Ppxlib

  let to_ocaml_exp = Selected_ast.To_ocaml.copy_expression
  let of_ocaml_exp = Selected_ast.Of_ocaml.copy_expression
end

(* let default_loc = !Ast_helper.default_loc *)
(* let mkloc ~loc txt = Location.{ txt; loc } *)
let mkloc ?(loc = !Ast_helper.default_loc) x = Location.mkloc x loc
let mknoloc = Location.mknoloc

let expr_mapper f str =
  let super = Ast_mapper.default_mapper in
  let self = { super with expr = f super } in
  self.structure self str

let untyp_expr_mapper f tstr =
  let super = Untypeast.default_mapper in
  let self = { super with expr = f super } in
  self.structure self tstr

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

(* ***************************************************** *)

open Parsetree

type class_name = string option
type hole = { hole_texp : Typedtree.expression; hole_cls : class_name }
type lpath = { level : int; path : Path.t }

type instance = {
  lpath : lpath;
  desc : Types.value_description;
  cls : class_name;
}

type ctx_instance = Mono of instance | Poly of instance

let mono_instance = "instance"
let poly_instance = "instance_with_context"

let show_instance inst =
  let show_cls = function None -> "None" | Some s -> s in
  let lpath_name lp = Path.name lp.path in
  match inst with
  | Mono inst | Poly inst ->
      Format.asprintf "(Class %s) %s : %a" (show_cls inst.cls)
        (lpath_name inst.lpath) Printtyp.type_expr inst.desc.val_type

let show_instances l =
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

exception Invalid_payload

let get_class : attribute -> class_name =
 fun attr ->
  match attr.attr_payload with
  | PStr [] -> None
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval ({ pexp_desc = Pexp_ident { txt = Lident id; _ }; _ }, []);
          _;
        };
      ] ->
      Some id
  | _ -> raise Invalid_payload

type handle_exc_inst = Include | Exclude

let collect_inst ~exc env name path desc acc =
  let get_inst inst_name attrs =
    let rec loop = function
      | [] -> None
      | attr :: attrs ->
          if attr.attr_name.txt = inst_name then
            Some
              (try get_class attr
               with Invalid_payload ->
                 Location.raise_errorf ~loc:attr.attr_loc
                   "(ppx_fillup) Illigal Instance payload: %a" Pprintast.payload
                   attr.attr_payload)
          else loop attrs
    in
    loop attrs
  in
  let types_derived =
    Env.fold_types
      (fun name _ decl acc ->
        if
          decl.type_attributes
          |> List.exists (fun attr -> attr.attr_name.txt = "deriving")
        then name :: acc
        else acc)
      None env []
  in
  let lpath = { level = 0; path } in
  Types.(
    match get_inst mono_instance desc.val_attributes with
    | Some cls -> Mono { lpath; desc; cls } :: acc
    | None -> begin
        match get_inst poly_instance desc.val_attributes with
        | Some cls -> Poly { lpath; desc; cls } :: acc
        | None -> (
            if
              types_derived
              |> List.exists (fun ty ->
                     Str.(
                       string_match
                         (regexp ("\\(show\\|pp\\|equal\\|compare\\)_" ^ ty))
                         name 0))
            then Mono { lpath; desc; cls = None } :: acc
            else
              match exc with
              | Include -> Mono { lpath; desc; cls = None } :: acc
              | Exclude -> acc)
      end)

let mkhole =
  let cnt = ref 0 in
  let open Ast_helper in
  fun ~loc ?(attrs = []) ?(payload = PStr []) () ->
    cnt := !cnt + 1;
    {
      (Cast.to_ocaml_exp
         [%expr
           (assert false
             : [%t
                 Ppxlib.Ast_helper.Typ.var @@ "fillup_hole" ^ string_of_int !cnt])])
      with
      pexp_attributes = Attr.mk ~loc { txt = "HOLE"; loc } payload :: attrs;
    }

let mkhole' ~loc ?(attrs = []) () = Cast.of_ocaml_exp @@ mkhole ~loc ~attrs ()
