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

(* open module as instance of ppx_fillup *)
let test_open_instances _ =
  let open%fillup M in
  assert_equal "true" @@ (show ## true);
  (* assert_equal "false" @@ show show_bool false;  <- Error *)
  ()

(* 3 or more arguments *)
let test_more_args _ =
  let pp_print_int[@inst] = Format.pp_print_int in
  assert_equal "123" @@ Format.asprintf "%a" __ 123

let _ =
  let tests =
    "Test fillup"
    >::: [
           "test show" >:: test_show;
           "test show polymorphic" >:: test_show_polymorphic;
           "test local declearation" >:: test_local_declearation;
           "test open module as instances" >:: test_open_instances;
           "test 3 or more args" >:: test_more_args;
         ]
  in
  run_test_tt_main tests
