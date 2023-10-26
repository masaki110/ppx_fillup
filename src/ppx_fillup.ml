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

(*** ppx rewriter ***)
let () =
  Ppxlib.Driver.(
    register_transformation
      ~extensions:
        Ppx_fillupsyntax.[ open_instance_toplevel; open_instance_local ]
      ~instrument:
        (Instrument.make Ppx_filluplib.Typeless.transform ~position:After)
      "ppx_fillup")
