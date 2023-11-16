(*** hole syntax ***)
let hole = Obj.magic 0

(*** default instance ***)
let (addii [@instance ( + )]) = ( + )
let (addff [@instance ( + )]) = ( +. )
let (addif [@instance ( + )]) = fun a b -> float_of_int a +. b
let (addfi [@instance ( + )]) = fun a b -> a +. float_of_int b
let (subii [@instance ( - )]) = ( - )
let (subff [@instance ( - )]) = ( -. )
let (subif [@instance ( - )]) = fun a b -> float_of_int a -. b
let (subfi [@instance ( - )]) = fun a b -> a -. float_of_int b
let (mulii [@instance ( * )]) = ( * )
let (mulff [@instance ( * )]) = ( *. )
let (mulif [@instance ( * )]) = fun a b -> float_of_int a *. b
let (mulfi [@instance ( * )]) = fun a b -> a *. float_of_int b
let (divii [@instance ( / )]) = ( / )
let (divff [@instance ( / )]) = ( /. )
let (divif [@instance ( / )]) = fun a b -> float_of_int a /. b
let (divfi [@instance ( / )]) = fun a b -> a /. float_of_int b

(*** default function ***)
let show (inst : 'a -> string) = inst
(* let print (show [@instance]) = ??print_endline *)
;;

Ppxlib.Driver.(
  register_transformation
    ~rules:
      Ppx_fillupsyntax.[ hole; open_instance_toplevel; open_instance_local ]
    ~instrument:
      (Instrument.make Ppx_filluplib.Typeless.transform ~position:After)
    (* ~impl:Ppx_filluplib.Typeless.transform  *)
    "ppx_fillup")
