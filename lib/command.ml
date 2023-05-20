open Syntax
open Typing

let red = "\027[31m"
let green = "\027[32m"
let reset = "\027[0m"

type 'env step_result = Continue of ty_env * 'env | Quit

let print_linenum (lexbuf : Lexing.lexbuf) =
  print_string "<<line : ";
  print_int lexbuf.lex_start_p.pos_lnum;
  print_string ">>"

let rec diff prev = function
  | [] -> []
  | (x, t) :: tl -> (
      match List.assoc_opt x prev with
      | Some t' when t = t' -> diff prev tl
      | _ -> (x, t) :: diff prev tl)

let set_tyenv tyenv (x, t) = (x, t) :: List.remove_assoc x tyenv

let print_diff tyenv tyenv' =
  List.iter
    (fun (x, t) -> print_endline ("--" ^ x ^ " : " ^ Print.string_of_type t))
    (diff tyenv tyenv')

let step filemode (lexbuf : Lexing.lexbuf) tyenv env =
  try
    let c = Parser.parse_command Lexer.tokenize lexbuf in
    match c with
    | None -> Quit
    | Some (CExp e) ->
        let t, tyenv = Typing.infer_expr tyenv e in
        print_endline
          ("<" ^ Print.string_of_type t ^ "> "
          ^ Print.string_of_value (Eval_name.eval env e));
        Continue (tyenv, env)
    | Some (CLet (x, e)) ->
        let t, tyenv = infer_expr tyenv e in
        let tyenv = set_tyenv tyenv (x, t) in
 (*       let v = Eval_name.eval env e in
        print_endline ("--" ^ x ^ " = " ^ Print.string_of_value v); *)
        Continue (tyenv, Eval_name.add_binding env (x, (env, e)))
    | Some (CRLet fs) ->
        let e = ERLet (fs, ETuple (List.map (fun (f, _, _) -> EVar f) fs)) in
        let t, tyenv = infer_expr tyenv e in
        let vartypes =
          match t with
          | TyTuple ts -> List.map2 (fun (f, _, _) ft -> (f, ft)) fs ts
          | _ -> failwith "unreachable"
        in
        let tyenv = List.fold_left set_tyenv tyenv vartypes in
        List.iter
          (fun (f, _, _) -> print_endline ("--" ^ f ^ " = rec function"))
          fs;
        Continue (tyenv, Eval_name.add_recfunction env fs)
    | Some (CTest (e, v)) ->
        let _, tyenv = infer_expr tyenv e in
        let r = Eval_name.eval env e in
        if r = v then print_endline ("--" ^ green ^ "ok" ^ reset)
        else (
          print_string ("--" ^ red ^ "failed " ^ reset);
          if filemode then print_linenum lexbuf;
          print_endline (" the result was " ^ Print.string_of_value r));
        Continue (tyenv, env)
  with Exception.Error msg ->
    if filemode then (
      print_linenum lexbuf;
      print_endline msg;
      Quit)
    else (
      print_endline msg;
      Lexing.flush_input lexbuf;
      Continue (tyenv, env))

let rec loop filemode lexbuf tyenv env =
  match step filemode lexbuf tyenv env with
  | Quit -> ()
  | Continue (tyenv', env') ->
      print_diff tyenv tyenv';
      loop filemode lexbuf tyenv' env'
