[@@@warnerror "-22"]

[@@@warnerror "-26"]

(* open Ppx_fillup_ppx_deriving *)
(* open Ppx_fillup_pp_print *)

(* print function *)
let print inst v = print_endline (inst v)

let (show_int [@instance]) = string_of_int
let () = print ## 123

let (show_list [@instance]) =
 fun inst xs -> String.concat "," (List.map inst xs)
let () = print ## [ 1; 2; 3 ]

(* ppx_deriving *)
type 'a pp = { pp : Format.formatter -> 'a -> unit } [@@typeclass]
let show (dict : 'a pp) v = Format.asprintf "%a\n" dict.pp v

type 'a equal = { equal : 'a -> 'a -> bool } [@@typeclass]
let equal (dict : 'a equal) v w = dict.equal v w

type 'a compare = { compare : 'a -> 'a -> int } [@@typeclass]
let compare (dict : 'a compare) v w = dict.compare v w

type student = { id : int; name : string } [@@deriving show, eq, ord, fillup]
let () = 
  let s1 = { id = 13; name = "Ito"} in
  let s2 = { id = 99; name = "Kato"} in
  print_endline show ## s1;
  print ## (equal [%HOLE] s1 s2);
  print ## (compare [%HOLE] s1 s2);

  


(*
   let () =
     print_int 123;  (* 123 *)
     print_float 1.0; (* 1.0 *)
     print_string "abc";
     print_endline (string_of_int 123);
     print_endline (string_of_float 1.0);
     print_endline "abc";

   type 'a showable = {show : 'a -> string}
   let print (dict:'a showable) v = print_string (dict.show v)

   (* print文 *)
   let (inst_int_show [@instance]) : int showable = {show = string_of_int}
   let (inst_float_show [@instance]) : float showable = {show = string_of_float}
   let (inst_string_show [@instance]) : string showable = {show = fun x -> x }
   let (inst_bool_show [@instance]) : bool showable = {show = string_of_bool}

   let () =
     print ## 123;
     print inst_int_show 123;
     print ## 1.0;
     print ## "abc";
     print [%HOLE] "abc";
     ()

   (* リストのprint文 *)
   let (inst_list_show [@instance]) : 'a showable -> 'a list showable =
     fun inner -> {show = fun xs -> "[ " ^ String.concat ", " (List.map inner.show xs) ^ " ]"}
   let () = print ## [1;2;3]

   (* 比較関数 *)
   type 'a equal = {eq : 'a -> 'a -> bool}
   let rec member equal x = function
     | [] -> false
     | y::ys -> equal.eq x y || member equal x ys
   let (inst_int_equal [@instance]) : int equal = {eq = Int.equal}
   let () = print ## ( member ## 1 [1;2;3])

   (* undecidable instance *)
   (* let (inst_int_list [@instance]) : int list showable =
     { show = fun xs -> String.concat "," (List.map string_of_int xs)} *)
   let () = print ## [1;2;3] *)
(*
   module type Show = sig
     type t
     val show : t -> string
   end

   let show {S:Show} x = S.show x

   implicit module Show_int = struct
     type t = int
     let show x = string_of_int x
   end
   implicit module Show_float = struct
     type t = float
     let show x = string_of_float x
   end

   implicit module Show_list {S : Show} = struct
     type t = S.t list
     let show x = string_of_list S.show x
   end

   let () =
     print_endline (show 5);
     print_endline (show 1.5);
     print_endline (show [1;2;3]); *)

(* 無限ループ *)
(* type 'a t = A of 'a
   type 'a c = { get : 'a }[@@typeclass]
   let (c1 [@instance]) : 'a t t c -> 'a t c = fun (_ : (('a t) t) c) -> { get = A (assert false) }

   let foo (_:'a t c) = ();; *)
(* foo [%HOLE] *)
