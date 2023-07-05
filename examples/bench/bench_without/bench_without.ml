print_endline (string_of_int 123);;
print_endline (string_of_float 1.23);;
print_endline "abc";;
List.iter print_int [ 1; 2; 3 ]

open! Ppxlib.Parsetree

let loc = Location.none;;

print_endline (Ppxlib.Pprintast.string_of_expression [%expr 1 + 1]);;
Format.printf "%d : %a" 42 Ppxlib.Pprintast.core_type [%type: int]

let mem = Base.List.mem;;

print_endline (string_of_bool (mem ~equal:Int.equal [ 1; 3; 5 ] 1));;
print_endline (string_of_bool (mem ~equal:Float.equal [ 1.2; 3.3; 5.3 ] 1.5))
