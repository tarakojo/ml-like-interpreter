open Syntax
open Typing

let red = "\027[31m"
let green = "\027[32m"
let reset = "\027[0m"

type step_res = Continue of ty_env * env | Quit

let print_error_msg msg = print_endline (red ^ "error" ^ reset ^ " : " ^ msg)
let string_of_type_suffix t = " : " ^ green ^ Print.string_of_type t ^ reset

let print_cexp_result v t =
  print_endline ("--> " ^ Print.string_of_value v ^ string_of_type_suffix t)

let print_linenum (lexbuf : Lexing.lexbuf) =
  print_string "<line : ";
  print_int lexbuf.lex_start_p.pos_lnum;
  print_string ">"

let print_ctest_result filemode lexbuf correct given =
  if correct = given then (
    if filemode then print_linenum lexbuf;
    print_endline (green ^ " ok" ^ reset))
  else (
    if filemode then print_linenum lexbuf;
    print_endline
      (red ^ " failed " ^ reset ^ "the result was "
      ^ Print.string_of_value given))

let tyenv_update tyenv (x, t) = (x, t) :: List.remove_assoc x tyenv

let rec tyenv_diff prev = function
  | [] -> []
  | (x, t) :: tl -> (
      match List.assoc_opt x prev with
      | Some t' when t = t' -> tyenv_diff prev tl
      | _ -> (x, t) :: tyenv_diff prev tl)

let print_tyenv_diff tyenv tyenv' =
  List.iter
    (fun (x, t) -> print_endline ("-- " ^ x ^ string_of_type_suffix t))
    (tyenv_diff tyenv tyenv')

let step filemode lexbuf tyenv env =
  try
    let c = Parser.parse_command Lexer.tokenize lexbuf in
    match c with
    | None -> Quit
    | Some (CExp e) ->
        let t, tyenv = Typing.infer_expr tyenv e in
        let v = (env, e) |> Eval.eval |> Eval.strict_eval in
        print_cexp_result v t;
        Continue (tyenv, env)
    | Some (CLet (x, e)) ->
        let t, tyenv = infer_expr tyenv e in
        let tyenv = tyenv_update tyenv (x, t) in
        Continue (tyenv, (x, BNonRec (env, e)) :: env)
    | Some (CRLet xs) ->
        let dummy_e = ERLet (xs, ETuple (List.map (fun (x, _) -> EVar x) xs)) in
        let dummy_t, tyenv = infer_expr tyenv dummy_e in
        let vartypes =
          match dummy_t with
          | TyTuple ts -> List.map2 (fun (x, _) xt -> (x, xt)) xs ts
          | _ -> assert false
        in
        let tyenv = List.fold_left tyenv_update tyenv vartypes in
        let bs = List.map (fun (x, ex) -> (x, BRec (ex, xs, env))) xs in
        Continue (tyenv, bs @ env)
    | Some (CTest (e, v)) ->
        let _, tyenv = infer_expr tyenv e in
        let r = (env, e) |> Eval.eval |> Eval.strict_eval in
        print_ctest_result filemode lexbuf v r;
        Continue (tyenv, env)
  with Exception.Error msg ->
    if filemode then (
      print_linenum lexbuf;
      print_error_msg msg;
      Quit)
    else (
      print_error_msg msg;
      Lexing.flush_input lexbuf;
      Continue (tyenv, env))

let rec loop filemode lexbuf tyenv env =
  match step filemode lexbuf tyenv env with
  | Quit -> ()
  | Continue (tyenv', env') ->
      print_tyenv_diff tyenv tyenv';
      loop filemode lexbuf tyenv' env'
