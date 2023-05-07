open Syntax

let red = "\027[31m"
let green = "\027[32m"
let reset = "\027[0m"

type step_result = Continue of env | Quit

let print_linenum (lexbuf : Lexing.lexbuf) =
  print_string "<<line : ";
  print_int lexbuf.lex_start_p.pos_lnum;
  print_string ">>"

let step filemode (lexbuf : Lexing.lexbuf) env =
  try
    let c = Parser.parse_command Lexer.tokenize lexbuf in
    match c with
    | None -> Quit
    | Some (CExp e) ->
        Print.print_value (Eval.eval env e);
        print_newline ();
        Continue env
    | Some (CLet (x, e)) ->
        let v = Eval.eval env e in
        print_endline ("--" ^ x ^ " = " ^ Print.string_of_value v);
        Continue ((x, v) :: env)
    | Some (CRLet fs) ->
        List.iter
          (fun (f, _, _) -> print_endline ("--" ^ f ^ " = rec function"))
          fs;
        Continue (Eval.add_recfunction env fs)
    | Some (CTest (e, v)) ->
        let r = Eval.eval env e in
        if r = v then print_endline ("--" ^ green ^ "ok" ^ reset)
        else (
          print_string ("--" ^ red ^ "failed " ^ reset);
          if filemode then print_linenum lexbuf;
          print_endline (" the result was " ^ Print.string_of_value r));

        Continue env
  with Exception.Error msg ->
    if filemode then (
      print_linenum lexbuf;
      print_endline msg;
      Quit)
    else (
      print_endline msg;
      Continue env)

let rec loop filemode lexbuf env =
  match step filemode lexbuf env with
  | Quit -> ()
  | Continue env' -> loop filemode lexbuf env'
