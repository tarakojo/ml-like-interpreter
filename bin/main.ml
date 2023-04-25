open Interpreter


let () =
  let filemode = Array.length Sys.argv > 1 in
  let lexbuf =
    if filemode then Lexing.from_channel (open_in Sys.argv.(1))
    else Lexing.from_channel stdin
  in

  Command.loop filemode lexbuf [] 
