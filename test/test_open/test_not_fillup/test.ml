[@@@warnerror "-33"]

type student = { id : int; name : string } [@@deriving show]

open Base

let () =
  let x = { id = 13; name = "ito" } in
  Caml.print_endline (Caml.string_of_int 1);
  Caml.print_endline (show_student x)
