(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]
;;

type foobar = {
  foo: int;
  bar: string;
}[@@deriving show,eq,show_fill]

let () =
  print_endline my_deriver;

(* 案1:deriving pluginに対応するものを手書きで作っておく --> とりあえず採用 *)

type 'a show = {show:'a -> string}[@@typeclass]
let show (dict:'a show) v = dict.show v
module Fillup = struct
  let _int[@instance] = {show=(fun x -> Printf.sprintf "\"%d\"" x)}
  let _foobar[@instance] = {show=(fun x -> show_foobar x)}
end
open Fillup

type 'a equal = {equal:'a -> 'a -> bool}[@@typeclass]
let equal (dict:'a equal) v w = dict.equal v w
let _foobar_eq[@instance] = {equal=(fun x y -> equal_foobar x y)}


(* let _list (inner:'a show) = {show = (fun xs -> String.concat ";" (List.map inner.show xs))} *)

(* let () =
  print_endline @@ show ## 1 *)

let () = 
  let x = {foo=1;bar="abc"} in
  print_endline @@ show ## x;
  print_endline @@ string_of_bool @@ equal ## x x;
  (* print_endline @@ show (_list _foobar) [x;x] *)


(* 
案2:##にderivingで生成される関数を入れる
  欠点:fillupで型が変わる可能性がある 
*)

(* let show (f: 'a -> string) = f

let () =
  let x = {foo=1;bar="abc"} in
  print_endline @@ show (show_foobar) x

let y =
  let _id x = x in
  let x = {foo=1;bar="abc"} in
  (* print_string @@ (assert false) x; *)
  print_string @@ (show_foobar) x *)
