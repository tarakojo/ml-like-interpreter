
open Interpreter
open Syntax

let rec loop lexbuf = 
    let c = Parser.parseCommand Lexer.tokenize lexbuf in 
    (match c with 
    | CExp e -> 
      Print.printValue (Eval.eval e); print_newline() ); interactiveMode lexbuf
let fileMode path = 
    let lexbuf = Lexing.from_channel (open_in path) in 
    let e = Parser.parseExpr Lexer.tokenize lexbuf in 
    Print.printValue (Eval.eval e); print_newline()

let () = 
    try 
      if Array.length Sys.argv <= 1 then 
        interactiveMode (Lexing.from_channel stdin)
      else
        fileMode (Sys.argv.(1)) 
        
    with
    | Parsing.Parse_error -> failwith "parse error"
    | _ -> failwith "error"
