(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]

open OUnit2

let printer x = x

(* Show *)
let show inst (v : 'a) = inst v
let (show_int [@instance]) = string_of_int
let (show_float [@instance]) = string_of_float
let (show_string [@instance]) = fun x : string -> x

let test_show _ =
  assert_equal ~printer "123" show ## 123;
  assert_equal ~printer "1.23" show ## 1.23;
  assert_equal ~printer "abc" show ## "abc"

let (show_list [@instance]) =
 fun inst xs ->
  match xs with [] -> "" | _ :: _ -> String.concat ", " (List.map inst xs)

let test_show_list _ =
  assert_equal "123, 456, 789" show ## [ 123; 456; 789 ];
  assert_equal "1.23, 4.56, 7.89" show ## [ 1.23; 4.56; 7.89 ];
  assert_equal "abc, def, ghi" show ## [ "abc"; "def"; "ghi" ]

(* deriving show *)
open Ppx_fillup_ppx_deriving

type student = { id : int; name : string } [@@deriving show, eq, ord, fillup]

let (show_bool [@instance]) = string_of_bool

let tests =
  "Test fillup"
  >::: [ "test show" >:: test_show; "test show list" >:: test_show_list ]

let _ = run_test_tt_main tests