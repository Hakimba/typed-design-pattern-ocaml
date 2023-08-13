type _ state =
  | Readable : in_channel -> [>`Readable] state
  | Eof : in_channel -> [>`Eof] state
  | Closed : in_channel -> [`Closed] state

let open_file path = Readable (open_in path)
let read_line (state : [`Readable] state) : string option * [`Readable | `Eof] state = match state with
  | Readable in_c -> 
    try 
      let line = input_line in_c in
      Some line, Readable in_c
    with
    End_of_file -> None,Eof in_c
         
let close_file (state: [< `Readable | `Eof] state) : [`Closed] state = match state with
    Readable ic | Eof ic -> close_in ic;Closed ic