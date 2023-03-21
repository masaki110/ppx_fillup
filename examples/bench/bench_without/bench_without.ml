(* let t0 = Benchmark.make 0L *)

type msg = Msg of string

let show_msg = function Msg x -> x

let (show_option [@instance_with_context]) =
 fun inst -> function None -> "None" | Some i -> "Some " ^ inst i

let (show_list [@instance_with_context]) =
 fun inst xs -> "[" ^ String.concat ", " (List.map inst xs) ^ "]"

type person = { id : int; name : string } [@@deriving show]
type affiliate = { name : string } [@@deriving show]

let _ =
  let msg = Msg "msg" in

  print_endline (string_of_int 123);
  print_endline (string_of_float 1.23);
  print_endline "abc";
  print_endline (string_of_bool true);

  print_endline (show_option string_of_int (Some 123));
  print_endline (show_option (fun x -> x) None);
  print_endline (show_list string_of_int [ 123; 456; 789 ]);
  print_endline
    (show_list (show_list string_of_float) [ [ 1.23; 4.56 ]; [ 7.89 ] ]);

  print_endline (show_msg msg);

  print_endline (show_person { id = 013; name = "ito" });
  print_endline (show_affiliate { name = "Gifu Univ." });

  let open! Ppxlib in
  let loc = Location.none in
  print_endline (Pprintast.string_of_expression [%expr 1 + 1]);
  Format.printf "%d : %a" 42 Pprintast.core_type [%type: int];
  ()

let _ =
  let mem = Base.List.mem in
  print_endline (string_of_bool (mem ~equal:Int.equal [ 1; 3; 5 ] 1));
  print_endline (string_of_bool (mem ~equal:Float.equal [ 1.2; 3.3; 5.3 ] 1.5));
  ()

(* let _ =
   let b = Benchmark.sub (Benchmark.make 0L) t0 in
   print_endline @@ "Benchmark results:" ^ Benchmark.to_string b *)
