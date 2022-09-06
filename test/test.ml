(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]

open OUnit2

(* Show *)
let show inst (v : 'a) = inst v
let (show_int [@instance]) = string_of_int
let (show_float [@instance]) = string_of_float
let (show_string [@instance]) = fun x : string -> x

let test_show _ =
  assert_equal "123" show ## 123;
  assert_equal "1.23" show ## 1.23;
  assert_equal "abc" show ## "abc"

let (show_option [@instance]) =
 fun inst -> function None -> "None" | Some i -> "Some " ^ inst i

let (show_list [@instance]) =
 fun inst xs -> String.concat ", " (List.map inst xs)

let test_show_polymorphic _ =
  assert_equal "Some 123" show ## (Some 123);
  assert_equal "None" show ## (None : string option);
  assert_equal "123, 456, 789" show ## [ 123; 456; 789 ];
  assert_equal "1.23, 4.56, 7.89" show ## [ 1.23; 4.56; 7.89 ]

module M = struct
  let (show_bool [@instance]) = string_of_bool
  let show_int2 = string_of_int
end

let test_local_declearation _ =
  let open M in
  assert_equal "true" @@ (show ## true);
  assert_equal "123" @@ (show ## 123)

let (float_float [@instance]) = fun x : float -> x
let (float_string [@instance]) = float_of_string
let (float_int [@instance]) = float_of_int
let add inst1 inst2 x y = inst1 x +. inst2 y

let () =
  print_float @@ add [%HOLE] [%HOLE] 4 "3";
  ()

(* open module for_ppx_fillup *)
module N = struct
  let (show_bool2 [@instance]) = string_of_bool
end

(* open%fillup M *)
let () =
  let open%fillup N in
  print_endline @@ (show ## true);
  ()

let _ =
  let tests =
    "Test fillup"
    >::: [
           "test show" >:: test_show;
           "test show polymorphic" >:: test_show_polymorphic;
           "test local declearation" >:: test_local_declearation
         ]
  in
  run_test_tt_main tests
