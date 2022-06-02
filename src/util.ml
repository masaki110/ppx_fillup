let mk_dummy_module =
  let cnt = ref 0 in
  fun () ->
    cnt := !cnt + 1;
    "Fillup_dummy_module" ^ string_of_int !cnt

let print_out x =
  let out = open_out @@ "/tmp/fillup_out" in
  output_string out x;
  close_out out

let mkloc ~loc txt =
  let open Ppxlib in
  { txt; loc }

let mknoloc txt = mkloc ~loc:!Ast_helper.default_loc txt