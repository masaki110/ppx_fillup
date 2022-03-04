(* ppx_deriving plugin *********************************************************)
(* Plugin : show *)
type 'a pp = { pp : Format.formatter -> 'a -> unit } [@@typeclass]

let show (dict : 'a pp) v = Format.asprintf "%a\n" dict.pp v

(* let _inst_show_tree[@instance] = fun (inner:'a pp) -> {pp=(fun x -> pp_tree inner.pp x)} *)

(* Plugin : eq *)
type 'a equal = { equal : 'a -> 'a -> bool } [@@typeclass]

let equal (dict : 'a equal) v w = dict.equal v w

(* let _foobar_eq[@instance] = {equal=(fun x y -> equal_foobar x y)}  *)

(* Plugin : ord *)
type 'a compare = { compare : 'a -> 'a -> int } [@@typeclass]

let compare (dict : 'a compare) v w = dict.compare v w

(* let _compare[@instance] = {compare=(fun x y -> compare_foobar x y)} *)

(* Plugin :enum  ( min and max are not necessary ) *)
type 'a to_enum = { to_enum : 'a -> int } [@@typeclass]

let to_enum (dict : 'a to_enum) v = dict.to_enum v

(* let _to_enum[@instance] = {to_enum=(fun x -> hogemoge_to_enum x)} *)

type 'a of_enum = { of_enum : int -> 'a option } [@@typeclass]

let of_enum (dict : 'a of_enum) v = dict.of_enum v

(* let _of_enum[@instance] = {of_enum=(fun x -> hogemoge_of_enum x)} *)

(* Plugin : iter *)

(* Plugin : map *)

(* Plugin : fold *)

(* Plugin : make *)

(* Print *)
let print (dict: 'a pp) v = Format.printf "%a\n" dict.pp v
