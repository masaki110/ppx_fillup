let (show_int [@instance_with_context]) = string_of_int
let (show_string [@instance_with_context]) = fun x : string -> x

(* let _ = print_endline (__ (__ (__ (__ (__ (__ (__ (__ (__ (__ 42)))))))))) *)
let _ = print_endline (__ (__ ""))
