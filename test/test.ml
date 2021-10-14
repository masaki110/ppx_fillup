(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]
;;

(* 案1:deriving pluginに対応するものを手書きで作っておく --> とりあえず採用 *)
open Ppx_fillup_plugin

type foobar = {
  foo: int;
  bar: string;
}[@@deriving show,eq,fillup]

let () = 
  let x = {foo=1;bar="abc"} in
  let y = {foo=2;bar="def"} in
  print_endline @@ show ## x;
  print_endline @@ string_of_bool @@ equal ## x y

(* type hogemoge = Hoge | Moge | Fuga [@@deriving enum]

type 'a of_enum = {of_enum: int -> 'a option}[@@typeclass]
let of_enum (dict:'a of_enum) v = dict.of_enum v
let _of_enum[@instance] = {of_enum=(fun x -> hogemoge_of_enum x)}

let () = 
  let _x = {foo=1;bar="abc"} in
  (* print_endline @@ show ## x; *)
  (* print_endline @@ show (_list _foobar) [x;x] *)
  (* print_endline @@ string_of_bool @@ equal ## _x _x; *)
  (* print_endline @@ string_of_int @@ compare ## _x _x; *)
  (* print_endline @@ string_of_int @@ to_enum ## Moge; *)
  match of_enum ## 0 with
  | Some Hoge -> print_endline "Hoge" 
  | Some Moge -> print_endline "Moge" 
  | Some Fuga -> print_endline "Fuga"
  | _ -> print_endline "None"  *)

(* let _list (inner:'a show) = {show = (fun xs -> String.concat ";" (List.map inner.show xs))} *)

(* let () =
  print_endline @@ show ## 1 *)


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
