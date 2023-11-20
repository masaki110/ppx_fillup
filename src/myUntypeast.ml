open Asttypes
open Parsetree
open Ast_helper
module T = Typedtree
open T
open Untypeast

let map_loc sub { loc; txt } = { loc = sub.location sub loc; txt }

let pattern : type k. _ -> k T.general_pattern -> _ =
 fun sub pat ->
  let loc = sub.location sub pat.pat_loc in
  (* todo: fix attributes on extras *)
  let attrs = sub.attributes sub pat.pat_attributes in
  let desc =
    match pat with
    | { pat_extra = [ (Tpat_unpack, loc, _attrs) ]; pat_desc = Tpat_any; _ } ->
        Ppat_unpack { txt = None; loc }
    | {
     pat_extra = [ (Tpat_unpack, _, _attrs) ];
     pat_desc = Tpat_var (_, name);
     _;
    } ->
        Ppat_unpack { name with txt = Some name.txt }
    | { pat_extra = [ (Tpat_type (_path, lid), _, _attrs) ]; _ } ->
        Ppat_type (map_loc sub lid)
    | { pat_extra = (Tpat_constraint ct, _, _attrs) :: rem; _ } ->
        Ppat_constraint
          (sub.pat sub { pat with pat_extra = rem }, sub.typ sub ct)
    | _ -> (
        match pat.pat_desc with
        | Tpat_any -> Ppat_any
        | Tpat_var (id, name) -> begin
            match (Ident.name id).[0] with
            | 'A' .. 'Z' -> Ppat_unpack { name with txt = Some name.txt }
            | _ -> Ppat_var name
          end
        (* We transform (_ as x) in x if _ and x have the same location.
           The compiler transforms (x:t) into (_ as x : t).
           This avoids transforming a warning 27 into a 26.
        *)
        | Tpat_alias ({ pat_desc = Tpat_any; pat_loc; _ }, id, name)
          when pat_loc = pat.pat_loc -> begin
            match (Ident.name id).[0] with
            | 'A' .. 'Z' -> Ppat_unpack { name with txt = Some name.txt }
            | _ -> Ppat_var name
          end
        | Tpat_alias (pat, _id, name) -> Ppat_alias (sub.pat sub pat, name)
        | Tpat_constant cst -> Ppat_constant (constant cst)
        | Tpat_tuple list -> Ppat_tuple (List.map (sub.pat sub) list)
        | Tpat_construct (lid, _, args, vto) ->
            let tyo =
              match vto with
              | None -> None
              | Some (vl, ty) ->
                  let vl =
                    List.map (fun x -> { x with txt = Ident.name x.txt }) vl
                  in
                  Some (vl, sub.typ sub ty)
            in
            let arg =
              match args with
              | [] -> None
              | [ arg ] -> Some (sub.pat sub arg)
              | args -> Some (Pat.tuple ~loc (List.map (sub.pat sub) args))
            in
            Ppat_construct
              ( map_loc sub lid,
                match (tyo, arg) with
                | Some (vl, ty), Some arg ->
                    Some (vl, Pat.mk ~loc (Ppat_constraint (arg, ty)))
                | None, Some arg -> Some ([], arg)
                | _, None -> None )
        | Tpat_variant (label, pato, _) ->
            Ppat_variant (label, Option.map (sub.pat sub) pato)
        | Tpat_record (list, closed) ->
            Ppat_record
              ( List.map
                  (fun (lid, _, pat) -> (map_loc sub lid, sub.pat sub pat))
                  list,
                closed )
        | Tpat_array list -> Ppat_array (List.map (sub.pat sub) list)
        | Tpat_lazy p -> Ppat_lazy (sub.pat sub p)
        | Tpat_exception p -> Ppat_exception (sub.pat sub p)
        | Tpat_value p -> (sub.pat sub (p :> pattern)).ppat_desc
        | Tpat_or (p1, p2, _) -> Ppat_or (sub.pat sub p1, sub.pat sub p2))
  in
  Pat.mk ~loc ~attrs desc

let default_mapper = { default_mapper with pat = pattern }