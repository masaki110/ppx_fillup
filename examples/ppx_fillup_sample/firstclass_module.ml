(* module Show = struct
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
   fun inst1 inst2 (a, b) -> "(" ^ inst1 a ^ ", " ^ inst2 b ^ ")"
end

module Eq = struct
  let eq_int = Int.equal
  let eq_float = Float.equal
  let eq_string = String.equal
end

open%fillup Show
open%fillup Eq

let print = print_endline

let digit_alist =
  [
    (0, "zero");
    (1, "one");
    (2, "two");
    (3, "three");
    (4, "four");
    (5, "five");
    (6, "six");
    (7, "seven");
    (8, "eight");
    (9, "nine");
  ]

let () =
  let open Base in
  !!print (List.Assoc.find ~!equal digit_alist 6);
  !!print (List.Assoc.find ~!equal digit_alist 22);
  !!print (List.Assoc.add ~!equal digit_alist 0 "zilech")

(* ************************************************** *)

module Mymod = struct
  open Base

  let myint = ((module Int) : _ Comparator.Module.t)
  let myfloat = ((module Float) : _ Comparator.Module.t)
end

open%fillup Mymod;;

Base.Set.of_list Mymod.myint [ 1; 2; 3 ];;
Base.Set.of_list Mymod.myfloat [ 1.; 2.; 3. ]

(* ************************************************** *)

open Ppxlib;;

let loc = Location.none in
Format.printf "%a is %a" Pprintast.expression [%expr 1 + 1] Pprintast.core_type
  [%type: int]

type student = { name : string; id : int } [@@deriving show]

(* ************************************************** *)

module Add = struct
  let addii = ( + )
  let addff = ( +. )
  let addif a b = float_of_int a +. b
  let addfi a b = a +. float_of_int b
end

let add inst = inst

open%fillup Add

type point2D = { x : float; y : float }

let (addpp [@instance]) =
 fun p1 p2 -> { x = ??add p1.x p2.x; y = ??add p1.y p2.y }
;;

{ x = 3.; y = 5. } == ??add { x = 1.; y = 2. } { x = 2.; y = 3. };;
[ 13; 35 ] = List.map (??add 1) [ 12; 34 ] *)
