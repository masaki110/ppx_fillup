open! OUnit2

module Add = struct
  let (addii [@instance ( + )]) = ( + )
  let (addff [@instance ( + )]) = ( +. )
  let (addif [@instance ( + )]) = fun a b -> float_of_int a +. b
  let (addfi [@instance ( + )]) = fun a b -> a +. float_of_int b
end

let arith _ =
  let open Add [@instance ( + )] in
  assert_equal 3 @@ (1 + 2);
  assert_equal 1.2 @@ (1.0 + 0.2);
  assert_equal 5.859 @@ (3.141 + 2.718)

module Show = struct
  let string_of_int = string_of_int
  let string_of_float = string_of_float

  let (string_of_list [@instance_with_context]) =
    fun f xs -> "[" ^ List.fold_left (fun acc x -> acc ^ f x ^ ";") "" xs ^ "]"
end

let typecast _ =
  let open Show [@instance ( !! )] in
  assert_equal "123" !!123;
  assert_equal "1.23" !!1.23;
  assert_equal "[123;456;]" !![ 123; 456 ]

open Base

module Eq = struct
  let eq_int = Int.equal
  let eq_float = Float.equal
end

let equal _ =
  let open Eq [@instance equal] in
  assert_equal true @@ List.mem ~equal [ 1.; 5.; 2. ] 1.

(* type foo =
  { bar : string
  ; baz : int
  }
[@@deriving show]

let deriving _ =
  let x = { bar = "ito"; baz = 022 } in
  Stdlib.Format.printf "%a" __ x;
  assert_equal "FF.{ Test.bar = 'ito'; baz = 22 }" (__ { bar = "ito"; baz = 022 }) *)

let expr _ =
  (* let (string_of_int [@instance ( !! )]) = Int.to_string in
     let (string_of_float [@instance ( !! )]) = Float.to_string in
     assert_equal "123" !!123;
     assert_equal "1.23" !!1.23; *)
  ()

let _ =
  let open OUnit2 in
  let tests =
    "Test fillup"
    >::: [ " hole_syntax: arithmetic" >:: arith
         ; " hole_syntax: typecast" >:: typecast
         ; " label: equal" >:: equal
         ; " local define of instance: " >:: expr (* ; " deriving" >:: deriving *)
         ]
  in
  run_test_tt_main tests
