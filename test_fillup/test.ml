(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]

type origint = int

open Base
open Ppx_fillup_typeclass
open Stdio

type int = origint

let (_inst_pp_int [@instance]) = { pp = Caml.Format.pp_print_int }

let (_inst_pp_bool [@instance]) = { pp = Caml.Format.pp_print_bool }

type foobar = { foo : int; bar : string } [@@deriving show, eq, ord, fillup]

let () =
  let x = { foo = 1; bar = "abc" } in
  let y = { foo = 2; bar = "def" } in
  print_endline @@ show##x;
  print_endline @@ (show ## (equal##x y));
  print_endline @@ (show ## (equal##x y));
  print_endline @@ (show ## (compare##x y));
  ()

type hogemoge = Hoge | Moge | Fuga [@@deriving enum, fillup]

let () =
  match of_enum ## 0 with
  | Some Hoge -> print_endline "Hoge"
  | Some Moge -> print_endline "Moge"
  | Some Fuga -> print_endline "Fuga"
  | _ ->
      print_endline "None";
      print_endline @@ (show ## (to_enum ## Hoge));
      ()

type 'a tree = Node of 'a tree * 'a tree | Leaf of 'a
[@@deriving show, iter, map, fold, fillup]

let x = Node (Node (Leaf 3, Node (Leaf 1, Leaf 2)), Leaf 4)

let x = Leaf x

let x = Leaf x

let x = Leaf x

let x = Leaf x;;

print_endline @@ show##x

(* include Core
   include Base
   module M = Base *)
