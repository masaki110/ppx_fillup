[@@@warnerror "-22"]

open Parsetree

open%fillup Pprintast

let loc = Location.none
let println x = Format.printf "%a\n" x

let () =
  println __ [%expr 1 + 2];
  println __ [%type: int];
  println __ [%pat? _, _];
  Format.printf "%a\n" __ [%pat? _, _];
