(* ppx_deriving plugin *********************************************************)
(* Plugin : show *)
type 'a pp = { pp : Format.formatter -> 'a -> unit }

let show (dict : 'a pp) v = Format.asprintf "%a" dict.pp v

(* Plugin : eq *)
type 'a equal = { equal : 'a -> 'a -> bool }

let equal (dict : 'a equal) v w = dict.equal v w

(* Plugin : ord *)
type 'a compare = { compare : 'a -> 'a -> int }

let compare (dict : 'a compare) v w = dict.compare v w

(* Plugin :enum  ( min and max are not necessary ) *)
type 'a to_enum = { to_enum : 'a -> int }

let to_enum (dict : 'a to_enum) v = dict.to_enum v

type 'a of_enum = { of_enum : int -> 'a option }

let of_enum (dict : 'a of_enum) v = dict.of_enum v

(* Plugin : iter *)

(* Plugin : map *)

(* Plugin : fold *)

(* Plugin : make *)
