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

let (print_list [@instance_with_context]) =
 fun inst xs -> "[" ^ String.concat ", " (List.map inst xs) ^ "]"

let id x = x

let test_show _ =
  let open%fillup Show in
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
  Format.printf "%d : %a" 42 __ [%type: int];
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

(* optional arguments *)

module Eq = struct
  let eq_int = Int.equal
  let eq_float = Float.equal
end

let test_optional _ =
  let mem = Base.List.mem in
  let open%fillup Eq in
  assert_equal true (mem ##~ equal [ 1; 3; 5 ] 1);
  assert_equal true (mem ##~ equal [ 1; 3; 5 ] 1);
  (* assert_equal (1.23 +. 4.56) (add __ 1.23 4.56);
     assert_equal ("foo" ^ "bar") (add __ "foo" "bar"); *)
  ()

(* first-class module *)
(* let first_class_module _ = *)
(* let sort (type s) (module Set : Set.S with type elt = s) l =
     Set.elements (List.fold_right Set.add l Set.empty)

   (* in *)
   let make_set (type s) cmp =
     let module S = Set.Make (struct
       type t = s

       let compare = cmp
     end) in
     (module S : Set.S with type elt = s)
   (* in *)

   let s = make_set
   let _ = sort (module ) s *)

(* ppx_deriving *)
type person = { id : int; name : string } [@@deriving show]
type affiliate = { name : string } [@@deriving show]

let ppx_deriving _ =
  let open%fillup Show in
  (* print_endline ## 123; *)
  assert_equal "{ id = 013; name = 'ito' }" id ## { id = 013; name = "ito" };
  assert_equal "{ name = 'Gifu Univ.' }" id ## { name = "Gifu Univ." };
  ()

let test_list_sort _ =
  let sort = List.sort [%derive.ord: int * int] in
  assert_equal ~printer:[%derive.show: (int * int) list]
    [ (1, 1); (2, 0); (3, 5) ]
    (sort [ (2, 0); (3, 5); (1, 1) ])

let _ =
  let tests =
    "Test fillup"
    >::: [
           " id" >:: test_show;
           " print Ast" >:: test_print_ast;
           " add" >:: test_add;
           (* " first-class module" >:: first_class_module; *)
           " ppx_deriving (show)" >:: ppx_deriving;
         ]
  in
  run_test_tt_main tests
