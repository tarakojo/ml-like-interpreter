open Syntax
open Exceptions

let print_binding name value_printer =
  print_string "-- ";
  print_string name;
  print_string " = ";
  value_printer ();
  print_newline ()

let run_command env c =
  match c with
  | CExp e ->
      Print.print_value (Eval.eval env e);
      print_newline ();
      env
  | CLet (x, e) ->
      let v = Eval.eval env e in
      print_binding x (fun () -> Print.print_value v);
      (x, v) :: env
  | CRLet fs ->
      List.iter
        (fun (f, _, _) ->
          print_binding f (fun () -> print_string "rec function"))
        fs;
      Eval.add_recfunction env fs

let msg_with_location filemode (lexbuf : Lexing.lexbuf) msg =
  if filemode then (
    print_string "###(line ";
    print_int lexbuf.lex_start_p.pos_lnum;
    print_string ", character ";
    print_int (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol + 1);
    print_endline ") ")
  else ();
  print_endline msg

let step filemode lexbuf env =
  try
    let c = Parser.parse_command Lexer.tokenize lexbuf in
    match c with None -> None | Some c -> Some (run_command env c)
  with
  | Parser.Error ->
      msg_with_location filemode lexbuf "syntax error";
      if filemode then None else Some env
  | SyntaxError msg ->
      msg_with_location filemode lexbuf ("syntax error : " ^ msg);
      if filemode then None else Some env
  | EvalError msg ->
      msg_with_location filemode lexbuf ("eval error : " ^ msg);
      if filemode then None else Some env
  | TypeError msg ->
      msg_with_location filemode lexbuf ("type error : " ^ msg);
      if filemode then None else Some env

let rec loop filemode lexbuf env =
  match step filemode lexbuf env with
  | None -> ()
  | Some env' -> loop filemode lexbuf env'
