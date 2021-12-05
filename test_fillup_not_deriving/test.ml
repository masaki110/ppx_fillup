open Ppx_fillup_typeclass

let (_inst_pp_int [@instance]) = { pp = Format.pp_print_int }

let () =
  print_endline (show ## 1);
  ()