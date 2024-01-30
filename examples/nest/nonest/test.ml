let (show [@instance]) = string_of_int
let (show [@instance]) = string_of_float

let (show [@rec_instance]) =
  fun xs -> "[" ^ List.fold_left (fun acc x -> acc ^ show x ^ "; ") " " xs ^ "]"
;;

print_endline @@ show 3;
print_endline @@ show 3.14

[@@@fillup]
(*
   without nest
   benchmark (10 times average)
   compile : 2.279s
*)
