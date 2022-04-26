type 'a showable = { pp : Format.formatter -> 'a -> unit }

let show dict v = Format.asprintf "%a" dict.pp v
let (inst_show_string [@instance]) = { pp = Format.pp_print_string }
let (inst_show_int [@instance]) = { pp = Format.pp_print_int }
let (inst_show_float [@instance]) = { pp = Format.pp_print_float }
let (inst_show_char [@instance]) = { pp = Format.pp_print_char }
let (inst_show_bool [@instance]) = { pp = Format.pp_print_bool }
