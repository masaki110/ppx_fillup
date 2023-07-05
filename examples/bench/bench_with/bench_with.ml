let (show_int [@instance]) = string_of_int
let (show_float [@instance]) = string_of_float
let (show_string [@instance]) = fun x : string -> x
let (show_bool [@instance]) = string_of_bool

let (show_list [@instance_with_context]) =
 fun f xs -> List.fold_left (fun acc x -> f x ^ acc) "" xs

let (eq_int [@instance]) = Int.equal
let (eq_float [@instance]) = Float.equal

open! Ppxlib.Parsetree

open%fillup Ppxlib.Pprintast

let mem = Base.List.mem
let loc = Location.none

let _ =
  print_endline ## 123;
  print_endline ## 1.23;
  print_endline ## "abc";
  print_endline ## [ 1; 2; 3 ];

  print_endline ## [%expr 1 + 1];
  Format.printf "%d : %a" 42 __ [%type: int];

  print_endline ## (mem ##~ equal [ 1; 3; 5 ] 1);
  print_endline ## (mem ##~ equal [ 1.2; 3.3; 5.3 ] 1.5)
