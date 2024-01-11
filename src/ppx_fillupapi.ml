open! Compatibility
open Parsetree

let hole_name = "HOLE"
let instance_name = "instance"
let instance_with_ctxt_name = "instance_with_context"
let dummy_prefix = "Ppx_fillup_instance"

exception Not_id
exception Not_hole
exception Invalid_payload of payload

let raise_errorf = Location.raise_errorf

let ( >>= ) op f =
  match op with
  | None -> None
  | Some x -> f x

let uniq x =
  let rec loop acc = function
    | [] -> List.rev acc
    | h :: t -> if List.mem h acc then loop acc t else loop (h :: acc) t
  in
  loop [] x

(* let string_of_list f l =
  let rec loop acc = function
    | [] -> "[]"
    | [ i ] -> acc ^ f i ^ " ]"
    | i :: rest -> loop (acc ^ f i ^ ",\n   ") rest
  in
  loop "[ " l *)

let string_of_list f xs = "[" ^ List.fold_left (fun acc x -> acc ^ f x ^ ";") "" xs ^ "]"

let string_of_option f = function
  | None -> "None"
  | Some op -> "Some of " ^ f op

let mangle ?(fixpoint = "t") affix name =
  match name = fixpoint, affix with
  | true, (`Prefix x | `Suffix x) -> x
  | true, `PrefixSuffix (p, s) -> p ^ "_" ^ s
  | false, `PrefixSuffix (p, s) -> p ^ "_" ^ name ^ "_" ^ s
  | false, `Prefix x -> x ^ "_" ^ name
  | false, `Suffix x -> name ^ "_" ^ x

let find_attr name attributes =
  match List.find (fun attr -> attr.attr_name.txt = name) attributes with
  | exception Not_found -> None
  | attribute -> Some attribute.attr_payload

let string_of_payload =
  let to_string = Format.asprintf in
  let open Pprintast in
  function
  | PStr st -> to_string "%a" structure st
  | PSig sg -> to_string "%a" signature sg
  | PTyp ty -> to_string "%a" core_type ty
  | PPat (pt, ex) ->
    (match ex with
     | None -> to_string "%a" pattern pt
     | Some ex -> to_string "%a when %a" pattern pt expression ex)

let rec string_of_lid =
  Longident.(
    function
    | Lident s -> s
    | Ldot (a, b) -> string_of_lid a ^ "." ^ b
    | Lapply (a, b) -> Printf.sprintf "%s(%s)" (string_of_lid a) (string_of_lid b))

let mkloc ?(loc = !Ast_helper.default_loc) x = Location.mkloc x loc
let mknoloc = Location.mknoloc

let id_of_texp = function
  | T.Texp_ident (_, { txt = Lident "__"; _ }, _) -> Some None
  | T.Texp_ident (_, { txt = Lident id; _ }, _) -> Some (Some id)
  | _ -> None

let id_of_payload = function
  | PStr [] ->
    (* overload '__' *)
    None
  | PStr
      [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_ident { txt = Lident id; _ }; _ }, _)
        ; _
        }
      ] ->
    (* overload ident *)
    Some id
  | payload ->
    (* otherwise *)
    raise (Invalid_payload payload)

let idopt_of_payload pl = Some (id_of_payload pl)

(* let evar ~loc ~attrs path =
  Ast_helper.Exp.ident ~loc ~attrs @@ mknoloc @@ lident_of_path path *)

(* used ppxlib **********************************)
open! Ppxlib
open! Parsetree

module To_current_ocaml = struct
  open Selected_ast.To_ocaml

  let expression = copy_expression
  let core_type = copy_core_type
  let pattern = copy_pattern
  let signature = copy_signature
  let structure = copy_structure

  let payload : payload -> Ppxlib_ast.Compiler_version.Ast.Parsetree.payload = function
    | PStr st -> PStr (structure st)
    | PSig sg -> PSig (signature sg)
    | PTyp ty -> PTyp (core_type ty)
    | PPat (pt, ex) ->
      (match ex with
       | None -> PPat (pattern pt, None)
       | Some ex -> PPat (pattern pt, Some (expression ex)))

  let attribute { attr_name; attr_payload; attr_loc } =
    Ppxlib_ast.Compiler_version.Ast.Parsetree.
      { attr_name; attr_payload = payload attr_payload; attr_loc }

  let attributes =
    let rec loop acc = function
      | [] -> List.rev acc
      | attr :: rest -> loop (attribute attr :: acc) rest
    in
    loop []
end

module Of_current_ocaml = struct
  open Selected_ast.Of_ocaml

  let expression = copy_expression
  let core_type = copy_core_type
  let pattern = copy_pattern
  let signature = copy_signature
  let structure = copy_structure

  let payload : Ppxlib_ast.Compiler_version.Ast.Parsetree.payload -> payload = function
    | PStr st -> PStr (structure st)
    | PSig sg -> PSig (signature sg)
    | PTyp ty -> PTyp (core_type ty)
    | PPat (pt, ex) ->
      (match ex with
       | None -> PPat (pattern pt, None)
       | Some ex -> PPat (pattern pt, Some (expression ex)))
end

let find_attr' name attrs =
  find_attr name (To_current_ocaml.attributes attrs)
  >>= fun pl -> Some (Of_current_ocaml.payload pl)

let string_of_payload' pl = string_of_payload @@ To_current_ocaml.payload pl
let id_of_payload' pl = id_of_payload (To_current_ocaml.payload pl)
let idopt_of_payload' pl = Some (id_of_payload' pl)

open Ast_helper

let mk_voidexpr =
  let cnt = ref 0 in
  fun ?(loc = !default_loc) ?(attrs = []) () ->
    cnt := !cnt + 1;
    { ([%expr (Stdlib.Obj.magic () : [%t Typ.var ("fillup" ^ string_of_int !cnt)])]) with
      pexp_attributes = attrs
    }

let mkhole ?(loc = !default_loc) ?(attrs = []) id =
  let id =
    match id with
    | None -> []
    | Some id -> [ Str.eval (Exp.ident { txt = Lident id; loc }) ]
  in
  let attrs = Attr.mk ~loc { txt = "HOLE"; loc } (PStr id) :: attrs in
  mk_voidexpr ~loc ~attrs ()

let mk_dummy_module =
  let cnt = ref 0 in
  fun () ->
    cnt := !cnt + 1;
    dummy_prefix ^ string_of_int !cnt

(* let id_of_stri = *)

let payload_of_id ~loc = function
  | None -> PStr []
  | Some id -> PStr [ Str.eval ~loc @@ Exp.ident ~loc { txt = Lident id; loc } ]

(* let instantiate_mb ~loc id mod_expr f =
  f
    ~loc
    ~attrs:[ Attr.mk ~loc { txt = instance_name; loc } (payload_of_id ~loc id) ]
    (mkloc ~loc @@ Some (mk_dummy_module ()))
    mod_expr *)

let stri_dummy_binding ~loc id mod_expr =
  Str.module_ ~loc
  @@ Mb.mk
       ~loc
       ~attrs:[ Attr.mk ~loc { txt = instance_name; loc } (payload_of_id ~loc id) ]
       { txt = Some (mk_dummy_module ()); loc }
       mod_expr

let expr_dummy_binding ~loc id mod_expr expr =
  Exp.letmodule
    ~loc
    ~attrs:[ Attr.mk ~loc { txt = instance_name; loc } (payload_of_id ~loc id) ]
    { txt = Some (mk_dummy_module ()); loc }
    mod_expr
    expr