open OUnit2

let (string_of_list [@rec_instance]) =
  fun show xs -> "[" ^ List.fold_left (fun acc x -> acc ^ show x ^ "; ") " " xs ^ "]"

let typecast _ =
  assert_equal "123" @@ string_of_int 123;
  assert_equal "1.23" @@ string_of_float 1.23;
  assert_equal "[ 123; 456; ]" @@ string_of_list string_of_int [ 123; 456 ]

let (add_pair [@rec_instance]) = fun addx addy (x1, y1) (x2, y2) -> addx x1 x2, addy y1 y2

let arith _ =
  assert_equal 3 @@ (1 + 2);
  assert_equal 1.2 @@ (float_of_int 1 +. 0.2);
  assert_equal (5.7, 9.14)
  @@ add_pair ( +. ) ( +. ) (float_of_int 3, 3.14) (2.7, float_of_int 6)

let first_class_module _ =
  let open Base in
  assert_equal 3 (Set.length (Set.of_list (module Int) [ 1; 2; 3 ]));
  assert_equal 3 (Set.length (Set.of_list (module Float) [ 1.23; 3.45; 6.78 ]));
  ()

let printast _ =
  let open Ppxlib in
  let loc = Location.none in
  assert_equal
    "1 + 1 : int"
    (Format.asprintf
       "%a : %a"
       Pprintast.expression
       [%expr 1 + 1]
       Pprintast.core_type
       [%type: int]);
  ()

type student =
  { name : string
  ; id : int
  }
[@@deriving show]

let deriving _ =
  let mydata = { name = "ito"; id = 022 } in
  assert_equal "{ Test.name = \"ito\"; id = 22 }" (show_student mydata);
  assert_equal "{ Test.name = \"ito\"; id = 22 }" (Format.asprintf "%a" pp_student mydata);
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
   benchmark (10 times average)
   compile: 1.232s
   exec: 0.193s
*)
