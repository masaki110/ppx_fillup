open! OUnit2

module Add = struct
  let addff = ( +. )
  let addii = ( + )
  let addif a b = float_of_int a +. b
  let addfi a b = a +. float_of_int b

  let (add_pair [@rec_instance]) =
    fun addx addy (x1, y1) (x2, y2) -> addx x1 x2, addy y1 y2
end

let arith _ =
  let open Add [@instance ( + )] in
  assert_equal 3 @@ (1 + 2);
  assert_equal 1.2 @@ (1 + 0.2);
  assert_equal (7, 5.84) @@ ((3, 3.14) + (4, 2.7))

(* assert_equal [ 3; 5 ] @@ ([ 1; 2 ] + [ 2; 3 ]) *)

module Show = struct
  let string_of_int = string_of_int
  let string_of_float = string_of_float

  let (string_of_list [@rec_instance]) =
    fun f xs -> "[" ^ List.fold_left (fun acc x -> acc ^ f x ^ ";") "" xs ^ "]"

  let (show_pair [@rec_instance]) =
    fun showx showy (x, y) -> "(" ^ showx x ^ "," ^ showy y ^ ")"
end

let typecast _ =
  let open Show [@instance ( !! )] in
  assert_equal "123" !!123;
  assert_equal "1.23" !!1.23;
  assert_equal "[123;456;]" !![ 123; 456 ]

let (show [@instance]) = string_of_int
let (show [@instance]) = string_of_float

let (show [@rec_instance]) =
  fun xs -> "[" ^ List.fold_left (fun acc x -> acc ^ show x ^ ";") "" xs ^ "]"

let (show [@rec_instance]) = fun (x, y) -> "(" ^ show x ^ "," ^ show y ^ ")"

let _ =
  print_endline @@ show [ 123 ];
  print_endline @@ show (123, 2.34)

let (( + ) [@instance]) = Int.add
let (( + ) [@instance]) = Float.add
let (( + ) [@instance]) = fun a b -> float_of_int a +. b
let (( + ) [@instance]) = fun a b -> a +. float_of_int b
let (( + ) [@rec_instance]) = fun (x1, y1) (x2, y2) -> x1 + x2, y1 + y2
let _ = print_int @@ (1 + 2)

open Base

module Eq = struct
  let eq_int = Int.equal
  let eq_float = Float.equal
end

let equal _ =
  let open Eq [@instance equal] in
  assert_equal true @@ List.mem ~equal [ 1.; 5.; 2. ] 1.

(* 出力 : [3.9;7.1] *)
(* type foo =
  { bar : string
  ; baz : int
  }
[@@deriving show]

let deriving _ =
  let x = { bar = "ito"; baz = 022 } in
  Stdlib.Format.printf "%a" __ x;
  assert_equal "FF.{ Test.bar = 'ito'; baz = 22 }" (__ { bar = "ito"; baz = 022 }) *)

let printast _ =
  let open Ppxlib in
  let open Parsetree in
  let open Pprintast [@instance] in
  let loc = Location.none in
  assert_equal
    "1 + 1 : int"
    (Stdlib.Format.asprintf "%a : %a" __ [%expr 1 + 1] __ [%type: int]);
  ()

module MyComp = struct
  open Base

  let myint = ((module Int) : _ Comparator.Module.t)
  let myfloat = ((module Float) : _ Comparator.Module.t)
  let mystring = ((module String) : _ Comparator.Module.t)
end

let first_class_module _ =
  let open%instance MyComp in
  assert_equal 3 (Set.length (Set.of_list __ [ 1; 2; 3 ]));
  assert_equal 3 (Set.length (Set.of_list __ [ 1; 2; 3 ]));
  assert_equal 3 (Set.length (Set.of_list __ [ 1.23; 3.45; 6.78 ]));
  ()

let _ =
  let open OUnit2 in
  let tests =
    "Test fillup"
    >::: [ " hole_syntax: arithmetic" >:: arith
         ; " hole_syntax: typecast" >:: typecast
         ; " label: equal" >:: equal
         ; " print AST: " >:: printast
         ; " first_class_module: Base.Set"
           >:: first_class_module (* ; " deriving" >:: deriving *)
         ]
  in
  run_test_tt_main tests
