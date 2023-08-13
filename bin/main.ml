(* Admin design pattern *)

let main_admin () =
  let open Tdpocaml in
  Admin.main ()

let main_state_m () =
  let open Tdpocaml.State_machine in
  let data = open_file "dune-project" in
  let rec loop d =
    match read_line d with
      Some line,Readable ic -> print_endline line; loop (Readable ic)
    | Some _, Eof ic -> Eof ic
    | None, Eof ic -> Eof ic
    | None, Readable ic -> Eof ic
  in
  let eof_state = loop data in
  (* Doesn't compile <- we try to read a file which is already fully readed
  let illegal_use = read_line eof_state in ...
  *)
  let closed_file = close_file eof_state in
  (* Doesn't compile <- we try to close an already closed file
  let illegal_use = close_file closed_file in illegal_use
  *)
  closed_file

let main_formatter () =
  let open Tdpocaml.Formatter in
  let tpl = FList.("Name : " ^ hole ^^ " Hobby : " ^ hole ^^ []) in
  let args = IdList.["hakim";"eating"] in
  print_endline (mprintf tpl args)
  (*
     Doesn't compile
  
  let tpl = FList.("Name " ^ hole ^^ "Hobby :" ^ hole ^^ []) in
  let too_much_args = IdList.["hakim";"eating";"yo"] in
  mprintf tpl args
  
  let tpl = FList.("Name " ^ hole ^^ "Hobby :" ^ hole ^^ []) in
  let not_enough_args = IdList.["hakim"] in
  mprintf tpl args

  *)

let _ = 
  let _ = main_admin () in
  let _ = main_state_m () in
  let _ = main_formatter () in ()