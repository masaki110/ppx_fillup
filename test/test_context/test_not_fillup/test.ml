type 'a tree = Node of 'a tree * 'a tree | Leaf of 'a [@@deriving show]

let () =
  let x = Leaf (Leaf (Leaf (Leaf (Node (Leaf 1, Leaf 2))))) in
  print_endline
    (show_tree (pp_tree (pp_tree (pp_tree (pp_tree Format.pp_print_int)))) x)
