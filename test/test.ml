(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]
;;

(* 案1:deriving pluginに対応するものを手書きで作っておく --> とりあえず採用 *)
open Ppx_fillup_plugin

(* to do
  1. derivingを利用しているopamプロジェクトを探す
  2. derivingの関数をfillup用のものに置き換える 
    e.g. print_endline @@ show_foobar x; -->  print_endline @@ show_foobar x;
  3. ベンチマーク
*)

type foobar = {
  foo: int;
  bar: string;
}[@@deriving show,eq,ord,fillup]

let () = 
  let x = {foo=1;bar="abc"} in
  let y = {foo=2;bar="def"} in
  print_endline @@ show ## x;
  (*print_endline @@ ((assert false)[@HOLE]) x; --> *)
  (* print_endline @@ show_foobar x; *)
  (* -->  print_endline @@ show_foobar x; *)
  let eq = (equal ## x y) in
  (* print_endline @@ show ## eq; *)
  print_endline @@ string_of_bool (equal ## x y);
  (* print_endline @@ string_of_bool @@ equal_foobar x y; *)
  print_endline @@ string_of_int @@ compare ## x y;
  ()

type 'a tree = Node of 'a tree * 'a tree | Leaf of 'a [@@deriving show,eq,ord,fillup]
(* type 'a forest = Trees of 'a tree * 'a tree [@@deriving show,eq,ord,fillup] *)

let () =
  (* let x = (Node (Leaf 1, Leaf 2)) in *)
  (* print_endline @@ show ## x; *)
  (* print_endline ## x; --> *)
  (* print_endline @@ (show_tree Format.pp_print_int) x; *)
  ()


(* let () =
  let x = (Node (Leaf 1, Leaf 2)) in
  let xs = (Trees ( x, x)) in
  print_endline @@ show (_inst_show_forest (_inst_show_tree ((assert false)[@HOLE]))) xs *)

(* type hogemoge = Hoge | Moge | Fuga [@@deriving enum]
let _of_enum[@instance] = {of_enum=(fun x -> hogemoge_of_enum x)}

let () = 
  match of_enum ## 0 with
  | Some Hoge -> print_endline "Hoge" 
  | Some Moge -> print_endline "Moge" 
  | Some Fuga -> print_endline "Fuga"
  | _ -> print_endline "None" *)
 
(*
  subtyping の説明
  [`A] <: [`A | `B]

  <a:int; b:string> <: <a:int>

  'a option は 型引数 'a について covariant かつ
  [`A] <: [`A | `B] なので
  [`A] option <: [`A | `B] option

  'a -> unit は 型引数 'a について contravariant かつ
  [`A] <: [`A | `B] なので
  [`A | `B] -> unit <: [`A] -> unit

*)
(* 
value restriction の説明

let r = ref None (* <- ここが式なので value restriction の対象になる*)
(* r : '_weak option ref <-- '_weak は単相的 *)

let () =
  r := Some 1 
ここで
(* r : int option ref *)
*)

(*
relaxed value restriction: covariant な型コンストラクタはvalue restrictionの対象にならない
*)

(* let r2 = Some (assert false) *)

(* let () =
  let x = (assert false)[@HOLE] (* forall 'a. 'a *)
  in
  print_int (x + 1);
  print_string x *)

(* let f = ((assert false)[@HOLE])

let g = show f 123 *)

(* type hogemoge = Hoge | Moge | Fuga [@@deriving enum]

type 'a of_enum = {of_enum: int -> 'a option}[@@typeclass] (* <-- covariant になってしまう！！*)
let of_enum (dict:'a of_enum) v = dict.of_enum v
let _of_enum[@instance] = {of_enum=(fun x -> hogemoge_of_enum x)} *)

(*
type ('a,'b) tree
[@@deriving show,fillup]

let inst_show_tree poly-a poly-b = {pp=(fun x -> pp_tree poly_a.pp poly_b.pp x)}

what is poly_fun_of_type_decl??

poly_fun_of_type_decl binop expr typedecl
==
((expr binop 'a') binop 'b')

List.fold_right biop (x1::(x2::(x3::[]))) e
==
x1 binop (x2 binop (x3 binop e))

(fun name expr -> [%expr [%e expr] [%e var name].pp])
*)


(* むりやり非covariantにするには, 抽象型にする*)
(* 
module N : sig
  type 'a of_enum
  val make_of_enum : (int -> 'a option) -> 'a of_enum
  val get_of_enum : 'a of_enum -> int -> 'a option
end = struct
  type 'a of_enum = {of_enum: int -> 'a option}
  let make_of_enum f = {of_enum=f}
  let get_of_enum {of_enum} = of_enum
end
open N
let of_enum (dict:'a of_enum) v = get_of_enum dict v
let _of_enum[@instance] = make_of_enum (fun x -> hogemoge_of_enum x) *)

(* let f x =
match x with
| Hoge -> print_endline "Hoge" 
| Moge -> print_endline "Moge" 
| Fuga -> print_endline "Fuga" *)
(* 
let () = 
  (* let x = of_enum (assert false) 0 in *)
  (* f (Option.get x); *)
  (* match x with
  | Some Hoge -> print_endline "Hoge" 
  | Some Moge -> print_endline "Moge" 
  | Some Fuga -> print_endline "Fuga"
  | _ -> print_endline "None"; *)
  match of_enum ## 0 with
  | Some Hoge -> print_endline "Hoge" 
  | Some Moge -> print_endline "Moge" 
  | Some Fuga -> print_endline "Fuga"
  | _ -> print_endline "None";
  (* match of_enum ## 0 with
  | Some Hoge -> print_endline "Hoge" 
  | Some Moge -> print_endline "Moge" 
  | Some Fuga -> print_endline "Fuga"
  | None -> print_endline "NONE" *)
  (* f (Option.get (of_enum ## 0)) *)
 *)


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
