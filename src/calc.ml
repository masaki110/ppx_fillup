module Add = struct
  let addii = ( + )
  let addff = ( +. )
  let addif a b = float_of_int a +. b
  let addfi a b = a +. float_of_int b
end

module Sub = struct
  let subii = ( - )
  let subff = ( -. )
  let subif a b = float_of_int a -. b
  let subfi a b = a -. float_of_int b
end

module Mul = struct
  let mulii = ( * )
  let mulff = ( *. )
  let mulif a b = float_of_int a *. b
  let mulfi a b = a *. float_of_int b
end

module Div = struct
  let mulii = ( / )
  let mulff = ( /. )
  let mulif a b = float_of_int a /. b
  let mulfi a b = a /. float_of_int b
end
