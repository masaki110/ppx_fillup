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
   fun show xs -> "[" ^ String.concat ", " (List.map show xs) ^ "]"

  let (show_pair [@instance_with_context]) =
   fun show1 show2 (a, b) -> "(" ^ show1 a ^ ", " ^ show2 b ^ ")"
end

let test_show _ =
  let open%fillup Show in
  assert_equal "123" (??show 123);
  assert_equal "3.14" (??show 3.14);
  assert_equal "Hello, World!" (??show "Hello, World!");
  assert_equal "true" (??show true);

  assert_equal "Some 123" (??show (Some 123));
  assert_equal "[123, 456, 789]" (??show [ 123; 456; 789 ]);
  assert_equal "[[1.23, 4.56], [7.89]]" (??show [ [ 1.23; 4.56 ]; [ 7.89 ] ]);
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
module MyComp = struct
  open Base

  let (myint [@instance]) =
    ((module Int) : (int, Int.comparator_witness) Base.Comparator.Module.t)

  let mystring = ((module String) : _ Comparator.Module.t)
end

let test_first_class_module _ =
  let open%fillup MyComp in
  let open! Base in
  assert_equal 3
    (Set.length
       (Set.of_list
          (__ : (int, Int.comparator_witness) Base.Comparator.Module.t)
          [ 1; 2; 3 ]));
  (* assert_equal 3 (Set.length (Set.of_list MyComp.myint [ 1; 2; 3 ])); *)
  (* assert_equal 3 (Set.length (Set.of_list __ [ 1.23; 3.45; 6.78 ])); *)
  ()

module type S = sig
  val x : int
end

let get_x (module X : S) = X.x

type ('a, 'b) either = Left of 'a | Right of 'b

let f takeleft x = takeleft (Left x)

let ((left [@instance]) : (int, string) either -> int) = function
  | Left x -> x
  | Right _ -> failwith "fail"

let _ = (f (__ : (int, string) either -> int) 100 : int)
let f = function Left x -> x | Right _ -> failwith "fail"
let ((left100 [@instance]) : (int, string) either) = Left 100
(* let _ = (f __ : int) *)

(***** ppx_deriving **********)
type point2D' = Pt of int * int [@@deriving show, eq, ord]

let plist = [ Pt (1, 2); Pt (5, 8); Pt (2, 9) ]

let test_deriving _ =
  let open%fillup Show in
  Base.List.(
    assert_equal "(Test.Pt (1, 2))" (??show (Pt (1, 2)));
    assert_equal true (mem ~!equal plist (Pt (1, 2)));
    ())

(****** arithmetic operation **********)
let test_arith _ =
  assert_equal 2 ([%H ( + )] 1 1);
  assert_equal [ 2; 4; 6 ] (List.map (fun x -> x * 2) [ 1; 2; 3 ]);
  assert_equal 6.28 (3.14 * 2);
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
         ]
  in
  run_test_tt_main tests

(* open Base

   let (myint [@instance]) =
     ((module Int) : (int, Int.comparator_witness) Base.Comparator.Module.t)

   let _ = Set.of_list __ [ 1; 2; 3 ] *)

let print ((inst [@instance]) : 'a -> string) (x : 'a) =
  print_endline (??show x)

(* let print ((inst [@instance]) : 'a -> string) (x : 'a) =
   print_endline (??show x) *)

let show inst = inst;;

let open%fillup Show in
(??show 123 : string)

let _ =
  let open%fillup Show in
  (* print Show.show_int 123; *)
  ??print 123;
  ()
