open OUnit2

let (show [@instance]) = string_of_int
let (show [@instance]) = string_of_float

let (show [@rec_instance]) =
  fun xs -> "[" ^ List.fold_left (fun acc x -> acc ^ show x ^ "; ") " " xs ^ "]"

let typecast _ =
  assert_equal "123" @@ show 123;
  assert_equal "1.23" @@ show 1.23;
  assert_equal "[ 123; 456; ]" @@ show [ 123; 456 ]

let (( + ) [@instance]) = ( + )
let (( + ) [@instance]) = ( +. )
let (( + ) [@instance]) = fun a b -> float_of_int a +. b
let (( + ) [@instance]) = fun a b -> a +. float_of_int b
let (( + ) [@rec_instance]) = fun (x1, y1) (x2, y2) -> x1 + x2, y1 + y2

let arith _ =
  assert_equal 3 @@ (1 + 2);
  assert_equal 1.2 @@ (1 + 0.2);
  assert_equal (5.7, 9.14) @@ ((3, 3.14) + (2.7, 6))

let first_class_module _ =
  let open Base in
  let (__ [@instance]) = ((module Int) : _ Comparator.Module.t) in
  let (__ [@instance]) = ((module Float) : _ Comparator.Module.t) in
  assert_equal 3 (Set.length (Set.of_list __ [ 1; 2; 3 ]));
  assert_equal 3 (Set.length (Set.of_list __ [ 1.23; 3.45; 6.78 ]));
  ()

let printast _ =
  let open Ppxlib in
  let open Pprintast [@instance pp] in
  let loc = Location.none in
  assert_equal "1 + 1 : int" (Format.asprintf "%a : %a" pp [%expr 1 + 1] pp [%type: int]);
  ()

type student =
  { name : string
  ; id : int
  }
[@@deriving show]

let deriving _ =
  let mydata = { name = "ito"; id = 022 } in
  assert_equal "{ Test.name = \"ito\"; id = 22 }" (show mydata);
  assert_equal "{ Test.name = \"ito\"; id = 22 }" (Format.asprintf "%a" show mydata);
  ()

let _ =
  let tests =
    "Test fillup"
    >::: [ " hole_syntax: arithmetic" >:: arith
         ; " hole_syntax: typecast" >:: typecast (* ; " label: equal" >:: equal *)
         ; " first_class_module: Base.Set" >:: first_class_module
         ; " module import: print AST" >:: printast
         ; " deriving" >:: deriving
         ]
  in
  run_test_tt_main tests

(*
   benchmark (10 times average) : 2.877s
*)
