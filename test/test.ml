(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]
;;

type 'a show = {show:'a -> string}[@@typeclass]
let show (dict:'a show) v = dict.show v
module Fillup = struct
  let _int[@instance] = {show=(fun x -> Printf.sprintf "\"%d\"" x)}
end

type foobar = {
  foo: int;
  bar: int;
}[@@deriving show]
(* 
let () =
  let open Fillup in
  print_endline @@ show ## 1

let () = 
  let x = {foo=1;bar=2} in
  print_endline @@ show ## x *) 