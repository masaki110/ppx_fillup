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

  let (show_option [@instance_with_context]) =
   fun inst -> function None -> "None" | Some i -> "Some " ^ inst i

  let (show_list [@instance_with_context]) =
   fun inst xs -> "[" ^ String.concat ", " (List.map inst xs) ^ "]"

  let (show_pair [@instance_with_context]) =
   fun inst1 inst2 (a, b) -> "(" ^ inst1 a ^ inst2 b ^ ")"
end

let id x = x

let test_show _ =
  let open%fillup Show in
  (* let (show_int [@instance]) = string_of_int in *)
  let msg = Show.(Msg "msg") in

  assert_equal "123" id ## 123;
  assert_equal "1.23" id ## 1.23;
  assert_equal "abc" id ## "abc";
  assert_equal "true" id ## true;

  assert_equal "Some 123" id ## (Some 123);
  assert_equal "None" id ## (None : string option);
  assert_equal "[123, 456, 789]" id ## [ 123; 456; 789 ];
  assert_equal "[[1.23, 4.56], [7.89]]" id ## [ [ 1.23; 4.56 ]; [ 7.89 ] ];

  assert_equal "msg" id##msg;
  ()

(* 3 or more arguments *)
(* print AST *)
let test_print_ast _ =
  let open! Ppxlib.Parsetree in
  let open%fillup Ppxlib.Pprintast in
  let loc = Location.none in
  assert_equal "1 + 1" id ## [%expr 1 + 1];
  assert_equal "42 : int" (Format.asprintf "%d : %a" 42 __ [%type: int]);
  ()

(* optional arguments *)
module Eq = struct
  let eq_int = Int.equal
  let eq_float = Float.equal
end

let test_optional _ =
  let mem = Base.List.mem in
  let open%fillup Eq in
  assert_equal true (mem ##~ equal [ 1; 3; 5 ] 1);
  assert_equal true (mem ##~ equal [ 1.2; 3.3; 5.3 ] 1.5);
  ()

(* first-class module *)
let sort (type s) set l =
  let module Set = (val set : Set.S with type elt = s) in
  Set.elements (List.fold_right Set.add l Set.empty)

let make_set (type s) cmp =
  let module S = Set.Make (struct
    type t = s

    let compare = cmp
  end) in
  (module S : Set.S with type elt = s)

let (set_int [@instance]) = make_set Int.compare
let (set_float [@instance]) = make_set Float.compare

let first_class_module _ =
  assert_equal [ 1; 2; 3 ] (sort __ [ 2; 1; 3 ]);
  assert_equal [ 1.3; 1.8; 2.3 ] (sort __ [ 2.3; 1.8; 1.3 ]);
  ()

(* ppx_deriving *)
type person = { id : int; name : string } [@@deriving show, eq, ord]
type affiliate = { name : string } [@@deriving show, eq, ord]

let equal ~eq x y = eq x y
let order ~ord x y = ord x y

let ppx_deriving _ =
  let open%fillup Show in
  assert_equal "{ Test.id = 13; name = \"ito\" }"
    id ## { id = 013; name = "ito" };
  assert_equal "{ Test.name = \"Gifu Univ.\" }" id ## { name = "Gifu Univ." };
  assert_equal true
    (equal ##~ eq { name = "Gifu Univ." } { name = "Gifu Univ." });
  assert_equal 0
    (order ##~ ord { id = 013; name = "ito" } { id = 013; name = "ito" });
  ()

(* arithmetic*)
(* module Arith = struct *)
let (add_int_int [@instance "+"]) = ( + )
let (add_int_float [@instance "+"]) = fun x y -> float_of_int x +. y
let (add_float_int [@instance "+"]) = fun x y -> x +. float_of_int y
let (add_float_float [@instance "+"]) = ( +. )
let (sub_int_int [@instance "-"]) = ( - )
let (sub_int_float [@instance "-"]) = fun x y -> float_of_int x -. y
let (sub_float_int [@instance "-"]) = fun x y -> x -. float_of_int y
let (sub_float_float [@instance "-"]) = ( -. )
(* end *)

let test_arith _ =
  (* let open%fillup Arith in *)
  (* assert_equal 3 (2 + 2 + 1); *)
  (* assert_equal ~-1. (2.5 - 1); *)
  ()

let _ =
  let tests =
    "Test fillup"
    >::: [
           " show" >:: test_show;
           " print Ast" >:: test_print_ast;
           " first-class module" >:: first_class_module;
           " ppx_deriving" >:: ppx_deriving;
           " arithmetic" >:: test_arith;
         ]
  in
  run_test_tt_main tests
