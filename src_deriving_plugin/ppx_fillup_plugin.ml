type 'a show = {show:'a -> string}[@@typeclass]
let show (dict:'a show) v = dict.show v

type 'a equal = {equal:'a -> 'a -> bool}[@@typeclass]
let equal (dict:'a equal) v w = dict.equal v w