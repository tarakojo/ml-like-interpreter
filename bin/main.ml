
open Interpreter
open Syntax

let rec interactiveMode (env : Eval.env) : unit = 
    let lexbuf = Lexing.from_channel stdin in 
    let c = Parser.parseCommand Lexer.tokenize lexbuf in 
    match c with 
    | CExp e -> 
      Print.printValue (Eval.eval env e); print_newline(); 
      interactiveMode env
    | CLet (x, e) -> 
      let v = Eval.eval env e in 
      print_string "-- " ;print_string x; print_string " = "; Print.printValue v; print_newline();
      interactiveMode ((x, v) :: env)
    | CRLet (f, x, e) -> 
      print_string "-- " ;print_string f; print_string " = "; print_endline "rec function";
      interactiveMode ((f, VRFun(f, x, e, env)) :: env)

let fileMode (path : string) : unit = 
    let lexbuf = Lexing.from_channel (open_in path) in 
    let e = Parser.parseExpr Lexer.tokenize lexbuf in 
    Print.printValue (Eval.eval [] e); print_newline()

let () =
    if Array.length Sys.argv <= 1 then 
      interactiveMode [] 
    else
      fileMode (Sys.argv.(1)) 
        
