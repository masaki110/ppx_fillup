[%%fillup open Pprintast]
open Parsetree
let loc = Location.none
let () =
  Format.printf "%a" __ ([%expr 1 + 1]);
  Format.printf "%a" __ ([%type : int])
