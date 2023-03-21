let (show_list [@instance_with_context]) =
 fun inst xs -> "[" ^ String.concat ", " (List.map inst xs) ^ "]"

let (show_int [@instance]) = string_of_int
let _ = print_endline ## [ [ [ [ [ [ [ [ [ [ 42 ] ] ] ] ] ] ] ] ] ]
