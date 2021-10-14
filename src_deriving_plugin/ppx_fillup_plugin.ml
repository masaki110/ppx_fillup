(* Plugin : show*)
type 'a show = {show:'a -> string}[@@typeclass]
let show (dict:'a show) v = dict.show v
(* let _show_foobar[@instance] = {show=(fun x -> show_foobar x)} *)

(* Plugin : eq *)
type 'a equal = {equal:'a -> 'a -> bool}[@@typeclass]
let equal (dict:'a equal) v w = dict.equal v w
(* let _foobar_eq[@instance] = {equal=(fun x y -> equal_foobar x y)}  *)

(* Plugin : ord *)
type 'a compare = {compare:'a -> 'a -> int}[@@typeclass]
let compare (dict:'a compare) v w = dict.compare v w
(* let _compare[@instance] = {compare=(fun x y -> compare_foobar x y)} *)

(* Plugin :enum  ( min and max are not necessary ) *)
type 'a to_enum = {to_enum:'a -> int}[@@typeclass]
let to_enum (dict:'a to_enum) v = dict.to_enum v
(* let _to_enum[@instance] = {to_enum=(fun x -> hogemoge_to_enum x)} *)


(* Plugin : iter *)
(* type 'a iter = {iter:'a -> unit}[@@typeclass] *)
(* let show (dict:'a show) v = dict.show v *)

(* Plugin : map *)

(* Plugin : fold *)

(* Plugin : make *)