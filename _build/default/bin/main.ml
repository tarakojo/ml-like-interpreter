
open Interpreter

let () = 
  try 
    let lexbuf = Lexing.from_channel stdin in 
    let v = Parser.parseExpr Lexer.tokenize lexbuf in 
    (match Eval.eval v with
    | VInt x -> print_int x 
    | VBool b -> print_string (if b then "true" else "false"))
    ; print_newline() 
  with
    | Parsing.Parse_error -> 
        print_endline "parse error" 