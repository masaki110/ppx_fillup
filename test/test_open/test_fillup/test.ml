[@@@warnerror "-33"]
(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]

open Ppx_fillup_typeclass
open Base

let (_inst_pp_int [@instance]) = { pp = Caml.Format.pp_print_int }

type student = { id : int; name : string } [@@deriving show, fillup]


let () =
  let x = { id = 13; name = "ito" } in
  Caml.print_endline show##1;
  Caml.print_endline show##x
