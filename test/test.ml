(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]

open OUnit2

let printer = fun x -> x

(* Show *)
let show inst (v : 'a) = (inst v)

let (show_int [@instance]) = string_of_int
let (show_float [@instance]) = string_of_float
let (show_string [@instance]) = fun x:string -> x

(* let tests_show = "test suite for Show" >::: [
  "show int" >:: (fun _ -> assert_equal "123" (show ## 123));
  "show float" >:: (fun _ -> assert_equal "1.23" (show ## 1.23));
  "show string" >:: (fun _ -> assert_equal "abc" (show ## "abc"));
] *)
let test_show _ = 
  assert_equal ~printer "123" (show ## 123);
  assert_equal ~printer "1.23" (show ## 1.23);
  assert_equal ~printer "abc" (show ## "abc") 

let (show_list [@instance]) =
  fun inst xs -> match xs with
    | [] -> ""
    | _::_ -> String.concat ", " (List.map inst xs)

let tests_show_list = "test suite for Show list" >::: [
  "show int list" >:: (fun _ -> assert_equal "123, 456, 789" (show ## [123;456;789]));
  "show float list" >:: (fun _ -> assert_equal "1.23, 4.56, 7.89" (show ## [1.23;4.56;7.89]));
  "show string list" >:: (fun _ -> assert_equal "abc, def, ghi" (show ## ["abc";"def";"ghi"]));
]
let _ = run_test_tt_main tests_show_list

(* deriving show *)
open Ppx_fillup_ppx_deriving

type student = { id : int; name : string } [@@deriving show, eq, ord, fillup]

let (show_bool [@instance]) = string_of_bool

(* let () =
let s1 = { id = 13; name = "Ito" } in
let s2 = { id = 99; name = "Kato" } in
print_endline show ## s1;
print ## (equal [%HOLE] s1 s2);
print ## (compare [%HOLE] s1 s2) *)

(*
(* undecidable instances *)
(* type 'a t = A of 'a
type 'a c = { get : 'a }[@@typeclass]
let (c1 [@instance]) : 'a t t c -> 'a t c = fun (_ : (('a t) t) c) -> { get = A (assert false) }

let foo (_:'a t c) = ();; *)
(* foo [%HOLE] *)

(* equal *)
type 'a equal = { eq : 'a -> 'a -> bool }

let rec member equal x = function
| [] -> false
| y :: ys -> equal.eq x y || member equal x ys

let (equal_int [@instance]) = { eq = Int.equal }

let () = print ## (member ## 1 [ 1; 2; 3 ]) *)

let tests = "Test fillup" >::: [
  "test show" >:: test_show;
]

let _ = run_test_tt_main tests