(* don't make preprocessor warnings as errors *)
(* [@@@warnerror "-22"] *)

open OUnit2

(* Show *)
module Show = struct
  type msg = Msg of string

  let show_int = string_of_int
  let show_float = string_of_float
  let show_string x : string = x
  let show_bool = string_of_bool
  let show_msg = function Msg x -> x
end

let (show_option [@instance_with_context]) =
 fun inst -> function None -> "None" | Some i -> "Some " ^ inst i

let (show_list [@instance_with_context]) =
 fun inst xs -> String.concat ", " (List.map inst xs)

let show inst (v : 'a) = inst v
let (msg [@instance]) = Show.show_msg

let test_show _ =
  let open%fillup Show in
  let msg = Show.(Msg "msg") in

  assert_equal "123" show ## 123;
  assert_equal "1.23" show ## 1.23;
  assert_equal "abc" show ## "abc";
  assert_equal "true" show ## true;

  assert_equal "Some 123" show ## (Some 123);
  assert_equal "None" show ## (None : string option);
  assert_equal "123, 456, 789" show ## [ 123; 456; 789 ];
  assert_equal "1.23, 4.56, 7.89" show ## [ [ 1.23; 4.56 ]; [ 7.89 ] ];
  assert_equal "msg" show##msg;
  ()

(* 3 or more arguments *)
(* print AST *)
let test_print_ast _ =
  let open Parsetree in
  let open%fillup Ppxlib.Pprintast in
  let loc = Location.none in
  assert_equal "1 + 1" show ## [%expr 1 + 1];
  assert_equal "42 : int" (Format.asprintf "%d : %a" 42 __ [%type: int]);
  ()

(* add *)
module Add = struct
  let add_int x y = x + y
  let add_float x y = x +. y
  let add_string x y = x ^ y
end

let add inst x y = inst x y

let test_add _ =
  let open%fillup Add in
  assert_equal (123 + 456) (add __ 123 456);
  assert_equal (1.23 +. 4.56) (add __ 1.23 4.56);
  assert_equal ("foo" ^ "bar") (add __ "foo" "bar");
  ()

let _ =
  let tests =
    "Test fillup"
    >::: [
           " show" >:: test_show;
           " print Ast" >:: test_print_ast;
           " add" >:: test_add;
         ]
  in
  run_test_tt_main tests
