(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]

[@@@warnerror "-26"]

(* 案1:deriving pluginに対応するものを手書きで作っておく --> とりあえず採用 *)
open Ppx_fillup_typeclass
open Ppx_fillup_pp_print

(* memo : 
 * typeclassと一緒にインスタンスを入れるとfillupしてくれないが、別のファイルに入れておくとfillupできる
 *)

(* to do
   1. derivingを利用しているopamプロジェクトを探す
   2. derivingの関数をfillup用のものに置き換える
     e.g. print_endline @@ show_foobar x; -->  print_endline @@ show_foobar x;
   3. ベンチマーク
*)


(* Plugin : show *)
type 'a g = { g : 'a -> string } [@@typeclass]
let f (dict : 'a g) x = dict.g x ^ "success"
let _inst_g [@instance] = {g=fun x -> x ^ " "};;
print_endline (f [%HOLE] "Overload") 

(* ppx_deriving *)

type foobar2 = { foo : int; bar : string } [@@deriving show]
type foobar3 = Foo | Bar [@@deriving enum];;

(* オーバーロード : show 関数 *)
show_foobar2 {foo=1;bar="ito"};;

show ## "abc";;

(* print_endline @@ (show ## (1,3)) *)

(* オーバーロード : ppx_deriving plugin *)
type foobar = { foo : int; bar : string } [@@deriving show, fillup]

type buz = { qux : bool } [@@deriving show, fillup];;

show ## { foo = 1; bar = "abc" };;

show ## { qux = true }

(* オーバーロード : equal instance *)
(* let _inst_equal[@instance] = Int.equal;;
   print_endline (show ## (Base.List.mem ## [1;2;3] 1));; *)

(* first class module *)
open Base

(* let _inst_mod_int[@instance] = (module Base__.Int);; *)
(* let s = Set.empty ((assert false  : 'a)[@HOlE]);;
   let s = Set.union s (Set.singleton ((assert false : 'a)[@HOlE]) 1);; *)

let s = Set.empty (module Int)

let s = Set.union s (Set.singleton (module Int) 1)

(* type foobar = { foo : int; bar : string } [@@deriving show, eq, ord, fillup]

   let () =
     let x = { foo = 1; bar = "abc" } in
     let y = { foo = 2; bar = "def" } in
     print_endline @@ show##x;
     print_endline @@ (show ## (equal##x y));
     print_endline @@ (show ## (compare##x y));
     () *)
(*
   type hogemoge = Hoge | Moge | Fuga [@@deriving enum,fillup]
   let () =
     match of_enum ## 0 with
     | Some Hoge -> print_endline "Hoge"
     | Some Moge -> print_endline "Moge"
     | Some Fuga -> print_endline "Fuga"
     | _ -> print_endline "None";
     ()

   type 'a tree = Node of 'a tree * 'a tree | Leaf of 'a [@@deriving show,iter,map,fold,fillup]
   type 'a iter = {iter: Format.formatter -> 'a -> unit}[@@typeclass]
   let iter (dict:'a iter) v = Format.asprintf "%a" dict.iter v
   (* let _inst_iter_tree[@instance] = fun (inner:'a pp) -> {iter=(fun x -> pp_tree inner.pp x)} *)
   let () =
     let x = (Node (Leaf 1, Leaf 2)) in
     iter_tree (Printf.printf "%d ") x; print_endline "";
     (* iter ## ((Printf.printf "%d ") x); print_endline ""; *)
     print_endline @@ show ## (map_tree ((+) 1) x);
     print_endline @@ show ## (fold_tree (+) 0 x);;
     ()

   let _instance[@instance] = string_of_int
   let f x y = x y
   (* [@@@typeclass : string -> int] *)
   let (g:string) = f ## 2 *)

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
