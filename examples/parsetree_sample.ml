[@@@warnerror "-22"]

open Parsetree

open%fillup Pprintast
open Pprintast

let loc = Location.none
let println x = Format.printf "%a\n" x

let () =
  println ## [%expr 1 + 2];
  println ## [%type: int];
  println ## [%pat? _, _];
  Format.printf "%a\n" __ [%pat? _, _];
