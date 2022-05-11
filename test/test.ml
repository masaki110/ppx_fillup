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

let (show_option [@instance]) =
 fun inst -> function None -> "None" | Some i -> "Some " ^ inst i

let (show_list [@instance]) =
 fun inst -> function [] -> "" | xs -> String.concat ", " (List.map inst xs)

let test_show_polymorphic _ =
  assert_equal "Some 123" show ## (Some 123);
  (* assert_equal "None" show ## None; *)
  assert_equal "123, 456, 789" show ## [ 123; 456; 789 ];
  assert_equal "1.23, 4.56, 7.89" show ## [ 1.23; 4.56; 7.89 ]

let test_local_declearation _ =
  let (show_bool [@instance]) = string_of_bool in
  assert_equal "true" show ## true

let (float_int [@instance]) = float_of_int

let average : int list -> float =
 fun xs ->
  let sum, count =
    xs |> List.fold_left (fun (sum, count) x -> (sum + x, succ count)) (0, 0)
  in
  [%HOLE] sum /. [%HOLE] count

(* deriving show *)
open Ppx_fillup_ppx_deriving

type student = { id : int; name : string } [@@deriving show, eq, ord, fillup]

let tests =
  "Test fillup"
  >::: [
         "test show" >:: test_show;
         "test show polymorphic" >:: test_show_polymorphic;
         "test local declearation" >:: test_local_declearation;
       ]

let _ = run_test_tt_main tests