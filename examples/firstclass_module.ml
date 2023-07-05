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
   fun inst1 inst2 (a, b) -> "(" ^ inst1 a ^ ", " ^ inst2 b ^ ")"
end

module Eq = struct
  let eq_int = Int.equal
  let eq_float = Float.equal
  let eq_string = String.equal
end

open%fillup Show
open%fillup Eq

open! Base

let print = Stdlib.print_endline

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

let _ =
  (* print "hoge"; *)
  !!print 123
(* print ## 123 *)
(* !print (List.Assoc.find ~?equal digit_alist 6) *)
(* print ## (List.Assoc.find ##~ equal digit_alist 6);
   print ## (List.Assoc.find ##~ equal digit_alist 22);
   print ## (List.Assoc.add ##~ equal digit_alist 0 "zilech") *)

(* let _ =
   (* let (myint [@instance]) = ((module Int) : (int, _) Base.Comparator.Module.t) in *)
   Set.to_list @@ Set.of_list (module Int) [ 1; 2; 3 ] *)
