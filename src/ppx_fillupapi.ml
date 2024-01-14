open! Compatibility
open Parsetree
module T = Typedtree

let hole_name = "HOLE"
let overload_name = "overload"
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

let string_of_list f xs =
  "[ " ^ List.fold_left (fun acc x -> acc ^ f x ^ "; ") "" xs ^ "]"

let string_of_option f = function
  | None -> "None"
  | Some op -> "Some of " ^ f op

let string_of_id id = string_of_option (fun x -> x) id

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

let string_of_path p = string_of_lid @@ lident_of_path p
let mkloc ?(loc = !Ast_helper.default_loc) x = Location.mkloc x loc
let mknoloc = Location.mknoloc

let id_of_texp texp =
  match texp.T.exp_desc with
  | Texp_ident (_, { txt = Lident "__"; _ }, _) -> None
  | Texp_ident (_, { txt = Lident id; _ }, _) -> Some id
  | _ -> raise Not_hole

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

let payload_of_id ~loc = function
  | None -> PStr []
  | Some id -> PStr [ Str.eval ~loc @@ Exp.ident ~loc { txt = Lident id; loc } ]

let mk_dummy_module =
  let cnt = ref 0 in
  fun () ->
    cnt := !cnt + 1;
    dummy_prefix ^ string_of_int !cnt

let mk_voidexpr =
  let cnt = ref 0 in
  fun ?(loc = !default_loc) ?(attrs = []) () ->
    cnt := !cnt + 1;
    (* { ([%expr Stdlib.Obj.magic ()]) with *)
    { ([%expr (Stdlib.Obj.magic () : [%t Typ.any ()])]) with
      (* { ([%expr (Stdlib.Obj.magic () : [%t Typ.var ("fillup" ^ string_of_int !cnt)])]) with *)
      pexp_attributes = attrs
    }

let id_binding ~loc name =
  Vb.mk
    ~loc
    (Pat.var
       ~loc
       ~attrs:[ Attr.mk ~loc (mkloc ~loc overload_name) (PStr []) ]
       { txt = name; loc })
    (mk_voidexpr ~loc ())

let instantiate_open ~loc id mod_expr =
  let name =
    match id with
    | None -> "__"
    | Some name -> name
  in
  Opn.mk ~loc
  @@ Mod.structure
       ~loc
       [ Str.module_ ~loc
         @@ Mb.mk
              ~loc
              ~attrs:[ Attr.mk ~loc { txt = instance_name; loc } (payload_of_id ~loc id) ]
              { txt = Some (mk_dummy_module ()); loc }
              mod_expr
       ; Str.value
           ~loc
           Nonrecursive
           [ Vb.mk
               ~loc
               (Pat.var
                  ~loc
                  ~attrs:[ Attr.mk ~loc (mkloc ~loc overload_name) (PStr []) ]
                  { txt = name; loc })
               (mk_voidexpr ~loc ())
           ]
       ]
