(* original *)
type foobar = {
  foo:int;
  bar:string
}[@@deriving show]

let () =
  print_endline @@ show_foobar {foo=1;bar="abc"}

(* use ppx_fillup *)
open Ppx_fillup_plugin

type hogemoge = {
  hoge:int;
  moge:string
}[@@deriving show,fillup]

let () =
  print_endline @@ show ## {hoge=1;moge="abc"}