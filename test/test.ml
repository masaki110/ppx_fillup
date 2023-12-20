[@@@warnerror "-26"]

let assert_equal = OUnit2.assert_equal
let show = Ppx_fillup.show

(***** Show **********)
module Show = struct
  let show_int = string_of_int
  let show_float = string_of_float
  let show_string x : string = x
  let show_bool = string_of_bool

  let (show_option [@instance_with_context]) =
   fun show -> function None -> "None" | Some i -> "Some " ^ show i

  let (show_list [@instance_with_context]) =
   fun inst xs ->
    "[" ^ List.fold_left (fun acc x -> acc ^ inst x ^ ";") "" xs ^ "]"
end

let test_show _ =
  (* let open%instance Show in *)
  let open%instance Show in
  assert_equal "123" (??show 123);
  assert_equal "3.14" (??show 3.14);
  assert_equal "Hello, World!" (??show "Hello, World!");
  assert_equal "true" (??show true);

  assert_equal "Some 123" (??show (Some 123));
  assert_equal "[123;456;789;]" (??show [ 123; 456; 789 ]);
  assert_equal "[[1.23;4.56;];[7.89;];]" (??show [ [ 1.23; 4.56 ]; [ 7.89 ] ]);
  ()

(****** print AST **********)
let test_print_ast _ =
  let open Ppxlib.Parsetree in
  let loc = Location.none in
  let open%instance Ppxlib.Pprintast in
  assert_equal "1 + 1 : int"
    (Format.asprintf "%a : %a" __ [%expr 1 + 1] __ [%type: int]);
  ()

(***** optional arguments **********)

module Eq = struct
  let eq_int = Int.equal
  let eq_float = Float.equal
  let eq_string = String.equal
  let eq_bool = Bool.equal
  let (eq_option [@instance_with_context]) = Option.equal
  let (show_list [@instance_with_context]) = List.equal

  let (show_pair [@instance_with_context]) =
   fun eq1 eq2 (a1, b1) (a2, b2) -> eq1 a1 a2 && eq2 b1 b2
end

let test_optional _ =
  let open%instance Eq in
  assert_equal true (Base.List.mem ~!equal [ (1, 2); (5, 8); (2, 9) ] (1, 2));
  ()

(***** first-class module **********)
module MyComp = struct
  open Base

  let myint = ((module Int) : _ Comparator.Module.t)
  let myfloat = ((module Float) : _ Comparator.Module.t)
  let mystring = ((module String) : _ Comparator.Module.t)
end

let test_first_class_module _ =
  let open%instance MyComp in
  let open! Base in
  assert_equal 3 (Set.length (Set.of_list __ [ 1; 2; 3 ]));
  assert_equal 3 (Set.length (Set.of_list __ [ 1; 2; 3 ]));
  assert_equal 3 (Set.length (Set.of_list __ [ 1.23; 3.45; 6.78 ]));
  ()

(***** ppx_deriving **********)
let test_deriving _ =
  let module M = struct
    type point2D = int * int [@@deriving show, eq]
  end in
  let open! M in
  assert_equal "(1, 2)" (??show (1, 2));
  assert_equal true (Base.List.mem ~!equal [ (1, 2); (5, 8); (2, 9) ] (1, 2));
  ()

(****** arithmetic operation **********)
let test_arith _ =
  (* let (add_pair [@instance_with_context ( + )]) =
      fun add1 add2 (x, y) (x', y') -> (add1 x x', add2 y y')
     in
     assert_equal (2, 3) (add_pair ( + ) ( + ) (1., 2.) (1, 1)); *)
  assert_equal 2 (1 + 1);
  assert_equal [ 2; 4; 6 ] (List.map (fun x -> x * 2) [ 1; 2; 3 ]);
  assert_equal 6.28 (3.14 * 2);
  assert_equal 6.28 (3.14 * 2.);
  ()

(****** shadowing **********)
let test_shadowing _ =
  let (_x [@instance]) = 123 in
  let (_x [@instance]) = "abc" in
  (* print_int (__ : int); *)
  ()

(********* run test ***************)
let _ =
  let open OUnit2 in
  let tests =
    "Test fillup"
    >::: [
           " show" >:: test_show;
           " print Ast" >:: test_print_ast;
           " first-class module" >:: test_first_class_module;
           " ppx_deriving" >:: test_deriving;
           " arithmetic" >:: test_arith;
           " shadowing" >:: test_shadowing;
         ]
  in
  run_test_tt_main tests
