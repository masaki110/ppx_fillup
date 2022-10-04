[@@@warnerror "-22"]

open Parsetree

open%fillup Pprintast

let loc = Location.none
let println x = Format.printf "%a\n" x

let () =
  println ## [%expr 1 + 2];
  println ## [%type: int];
  println ## [%pat? _, _]
