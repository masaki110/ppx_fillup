(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]

open Ppx_fillup_typeclass

let (_inst_pp_int [@instance]) = { pp = Format.pp_print_int }

type 'a tree = Node of 'a tree * 'a tree | Leaf of 'a
[@@deriving show, fillup]

let () =
  let x = Leaf (Leaf (Leaf (Leaf (Node (Leaf 1, Leaf 2))))) in
  print_endline show##x
