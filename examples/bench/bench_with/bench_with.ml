type msg = Msg of string

module Show = struct
  let show_int = string_of_int
  let show_float = string_of_float
  let show_string x : string = x
  let show_bool = string_of_bool
  let show_msg = function Msg x -> x

  let (show_option [@instance_with_context]) =
   fun inst -> function None -> "None" | Some i -> "Some " ^ inst i

  let (show_list [@instance_with_context]) =
   fun inst xs -> "[" ^ String.concat ", " (List.map inst xs) ^ "]"
end

type person = { id : int; name : string } [@@deriving show]
type affiliate = { name : string } [@@deriving show]

open%fillup Show

let _ =
  let msg = Msg "msg" in

  print_endline ## 123;
  print_endline ## 1.23;
  print_endline ## "abc";
  print_endline ## true;

  print_endline ## (Some 123);
  print_endline ## (None : string option);
  print_endline ## [ 123; 456; 789 ];
  print_endline ## [ [ 1.23; 4.56 ]; [ 7.89 ] ];

  print_endline##msg;

  print_endline ## { id = 013; name = "ito" };
  print_endline ## { name = "Gifu Univ." };

  let open! Ppxlib.Parsetree in
  let open%fillup Ppxlib.Pprintast in
  let loc = Location.none in
  print_endline ## [%expr 1 + 1];
  Format.printf "%d : %a" 42 __ [%type: int];
  ()

module Eq = struct
  let eq_int = Int.equal
  let eq_float = Float.equal
end

let _ =
  let mem = Base.List.mem in
  let open%fillup Eq in
  print_endline ## (mem ##~ equal [ 1; 3; 5 ] 1);
  print_endline ## (mem ##~ equal [ 1.2; 3.3; 5.3 ] 1.5);
  ()
