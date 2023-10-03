(* don't make preprocessor warnings as errors *)
(* [@@@warnerror "-22"] *)

open OUnit2

(***** Show **********)
module Show = struct
  let show_int = string_of_int
  let show_float = string_of_float
  let show_string x : string = x
  let show_bool = string_of_bool

  let (show_option [@instance_with_context]) =
   fun show -> function None -> "None" | Some i -> "Some " ^ show i

  let (show_list [@instance_with_context]) =
   fun show xs -> "[" ^ String.concat ", " (List.map show xs) ^ "]"

  let (show_pair [@instance_with_context]) =
   fun show1 show2 (a, b) -> "(" ^ show1 a ^ show2 b ^ ")"
end

let show x : string = x

let test_show _ =
  let open%fillup Show in
  assert_equal "123" (!!show 123);
  assert_equal "3.14" (!!show 3.14);
  assert_equal "Hello, World!" (!!show "Hello, World!");
  assert_equal "true" (!!show true);

  assert_equal "Some 123" (!!show (Some 123));
  assert_equal "[123, 456, 789]" (!!show [ 123; 456; 789 ]);
  assert_equal "[[1.23, 4.56], [7.89]]" (!!show [ [ 1.23; 4.56 ]; [ 7.89 ] ]);
  ()

(****** print AST **********)
let test_print_ast _ =
  let open Ppxlib.Parsetree in
  let loc = Location.none in
  let open%fillup Ppxlib.Pprintast in
  assert_equal "1 + 1 : int"
    (Format.asprintf "%a : %a" __ [%expr 1 + 1] __ [%type: int]);
  ()

(***** optional arguments **********)
type point2D = Pt of int * int

let (equal_point2D [@instance]) =
 fun (Pt (x1, y1)) (Pt (x2, y2)) -> x1 = x2 && y1 = y2

let plist = [ Pt (1, 2); Pt (5, 8); Pt (2, 9) ]

let test_optional _ =
  let open Base.List in
  assert_equal true (mem ~!equal plist (Pt (1, 2)));
  ()

(***** first-class module **********)
module Mymod = struct
  open Base

  let myint = ((module Int) : _ Comparator.Module.t)
  let myfloat = ((module Float) : _ Comparator.Module.t)
end

let test_first_class_module _ =
  let open Base.Set in
  let open Mymod in
  assert_equal [ 1; 2; 3 ] (to_list (of_list myint [ 1; 2; 3 ]));
  assert_equal [ 1.2; 3.4; 5.6 ] (to_list (of_list myfloat [ 1.2; 3.4; 5.6 ]))

(***** ppx_deriving **********)
type point2D' = Pt of int * int [@@deriving show, eq, ord]

let plist = [ Pt (1, 2); Pt (5, 8); Pt (2, 9) ]

let test_deriving _ =
  let open Base.List in
  let open%fillup Show in
  assert_equal "(1,2)" (!!show (Pt (1, 2)));
  assert_equal true (mem ~!equal plist (Pt (1, 2)));
  ()

(****** arithmetic operation **********)
let test_arith =
  (* let open Calc in
     assert_equal 2 (1 + 1);
     assert_equal 6.28 (3.14 * 2);
     assert_equal [ 2; 4; 6 ] (List.map (fun x -> x * 2) [ 1; 2; 3 ]); *)
  ()

(********* run test ***************)
let _ =
  let tests =
    "Test fillup"
    >::: [
           " show" >:: test_show;
           " print Ast" >:: test_print_ast;
           " first-class module" >:: test_first_class_module;
           " ppx_deriving" >:: test_deriving;
           (* " arithmetic" >:: test_arith; *)
         ]
  in
  run_test_tt_main tests
