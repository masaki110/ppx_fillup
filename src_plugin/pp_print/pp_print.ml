type 'a showable = { pp : Format.formatter -> 'a -> unit }

let show (dict : 'a showable) v = Format.asprintf "%a" dict.pp v

let (_inst_pp_string [@instance]) = { pp = Format.pp_print_string }

let (_inst_pp_int [@instance]) = { pp = Format.pp_print_int }

let (_inst_pp_float [@instance]) = { pp = Format.pp_print_float }

let (_inst_pp_char [@instance]) = { pp = Format.pp_print_char }

let (_inst_pp_bool [@instance]) = { pp = Format.pp_print_bool }