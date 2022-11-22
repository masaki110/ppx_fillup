[%%fillup open Pprintast]
open Parsetree
let loc = Location.none
let pp_exp = Pprintast.expression[@@instance ]
let pp_typ = Pprintast.core_type[@@instance ]
let () =
  Format.printf "%a\n" __ ([%expr 1 + 1]);
  Format.printf "%a\n" __ ([%type : int]);
  Format.printf "%a\n" __ ([%expr 1 + 1]);
  Format.printf "%a\n" __ ([%type : int])
