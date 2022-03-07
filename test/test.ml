(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]

(* print function *)
let print inst v = print_endline (inst v)

let (show_int [@instance]) = string_of_int

let (show_float [@instance]) = string_of_float

let () =
  print ## 123;
  print ## 1.23

let (show_list [@instance]) =
 fun inst xs -> String.concat "," (List.map inst xs)

let () = print ## [ 1; 2; 3 ]

(* ppx_deriving *)
open Ppx_fillup_ppx_deriving

type student = { id : int; name : string } [@@deriving show, eq, ord, fillup]

let (show_bool [@instance]) = string_of_bool

let () =
  let s1 = { id = 13; name = "Ito" } in
  let s2 = { id = 99; name = "Kato" } in
  print_endline show ## s1;
  print ## (equal [%HOLE] s1 s2);
  print ## (compare [%HOLE] s1 s2)

(* undecidable instances *)
(* type 'a t = A of 'a
   type 'a c = { get : 'a }[@@typeclass]
   let (c1 [@instance]) : 'a t t c -> 'a t c = fun (_ : (('a t) t) c) -> { get = A (assert false) }

   let foo (_:'a t c) = ();; *)
(* foo [%HOLE] *)

(* equal *)
type 'a equal = { eq : 'a -> 'a -> bool }

let rec member equal x = function
  | [] -> false
  | y :: ys -> equal.eq x y || member equal x ys

let (equal_int [@instance]) = { eq = Int.equal }

let () = print ## (member ## 1 [ 1; 2; 3 ])
