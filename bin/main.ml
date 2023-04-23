open Interpreter
open Syntax

let runCommand env c =
  try
    match c with
    | CExp e ->
        Print.printValue (Eval.eval env e);
        print_newline ();
        env
    | CLet (x, e) ->
        let v = Eval.eval env e in
        print_string "-- ";
        print_string x;
        print_string " = ";
        Print.printValue v;
        print_newline ();
        (x, v) :: env
    | CRLet (f, x, e) ->
        print_string "-- ";
        print_string f;
        print_string " = ";
        print_endline "rec function";
        (f, VRFun (f, x, e, env)) :: env
  with
  | Parsing.Parse_error ->
      print_endline "parse error";
      env
  | Eval.Eval_error ->
      print_endline "eval error";
      env

let () =
  if Array.length Sys.argv <= 1 then
    let rec loop env lexbuf =
      let c = Parser.parseCommand Lexer.tokenize lexbuf in
      loop (runCommand env c) lexbuf
    in
    loop [] (Lexing.from_channel stdin)
  else
    let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
    let cs = Parser.parseFile Lexer.tokenize lexbuf in
    ignore (List.fold_left (fun env c -> runCommand env c) [] cs)

(*
  if Array.length Sys.argv <= 1 then loop [] (Lexing.from_channel stdin)
  else loop [] (Lexing.from_channel (open_in Sys.argv.(1)))
  
let rec loop (env : Eval.env) lexbuf : unit =
  let c = Parser.parseCommand Lexer.tokenize lexbuf in
  let env' =
    try
      match c with
      | CExp e ->
          Print.printValue (Eval.eval env e);
          print_newline ();
          Some env
      | CLet (x, e) ->
          let v = Eval.eval env e in
          print_string "-- ";
          print_string x;
          print_string " = ";
          Print.printValue v;
          print_newline ();
          Some ((x, v) :: env)
      | CRLet (f, x, e) ->
          print_string "-- ";
          print_string f;
          print_string " = ";
          print_endline "rec function";
          Some ((f, VRFun (f, x, e, env)) :: env)
    with
    | Parsing.Parse_error ->
        print_endline "parse error";
        None
    | Eval.Eval_error ->
        print_endline "eval error";
        None
  in
  match env' with Some env' -> loop env' lexbuf | None -> loop env lexbuf

   *)
