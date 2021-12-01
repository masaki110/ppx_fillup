type foobar = {
  foo: int;
  bar: string;
}[@@deriving show,eq,ord]
let () = 
  let x = {foo=1;bar="abc"} in
  let y = {foo=2;bar="def"} in
  print_endline @@ show_foobar x;
  print_endline @@ string_of_bool (equal_foobar x y);
  print_endline @@ string_of_int (compare_foobar x y);
  ()

type hogemoge = Hoge | Moge | Fuga [@@deriving enum]
let () = 
  match hogemoge_of_enum 0 with
  | Some Hoge -> print_endline "Hoge" 
  | Some Moge -> print_endline "Moge" 
  | Some Fuga -> print_endline "Fuga"
  | _ -> print_endline "None";
  print_endline @@ string_of_int (hogemoge_to_enum Hoge);
  ()

type 'a tree = Node of 'a tree * 'a tree | Leaf of 'a[@@deriving show,iter,map,fold]

let x = Node (Node (Leaf 3, Node (Leaf 1, Leaf 2)), Leaf 4);;
let x = Leaf x;;
let x = Leaf x;;
let x = Leaf x;;
let x = Leaf x;;
print_endline @@ show_tree (pp_tree (pp_tree (pp_tree (pp_tree Format.pp_print_int)))) x;;
