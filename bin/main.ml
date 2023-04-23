open Interpreter
open Syntax

let print_binding name value_printer =
  print_string "-- ";
  print_string name;
  print_string " = ";
  value_printer ();
  print_newline ()

let run_command env c =
  try
    match c with
    | CExp e ->
        Print.printValue (Eval.eval env e);
        print_newline ();
        env
    | CLet (x, e) ->
        let v = Eval.eval env e in
        print_binding x (fun () -> Print.printValue v);
        (x, v) :: env
    | CRLet fs ->
        List.iter
          (fun (f, _, _) ->
            print_binding f (fun () -> print_string "rec function"))
          fs;
        Eval.add_recfunction env fs
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
      let c = Parser.parse_command Lexer.tokenize lexbuf in
      loop (run_command env c) lexbuf
    in
    loop [] (Lexing.from_channel stdin)
  else
    let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
    let cs = Parser.parse_file Lexer.tokenize lexbuf in
    ignore (List.fold_left (fun env c -> run_command env c) [] cs)
