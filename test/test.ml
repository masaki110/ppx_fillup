(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]
;;


type 'a show = {show:'a -> string}[@@typeclass]
let show (dict:'a show) v = dict.show v

module M = struct
  let _int[@instance] = {show=(fun x -> Printf.sprintf "\"%d\"" x)}
end

let () =
  let open! M in
  print_endline @@ show ## 1