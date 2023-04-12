
(* The type of tokens. *)

type token = 
  | TRUE
  | THEN
  | SUB
  | MUL
  | LT
  | INT of (int)
  | IF
  | FALSE
  | EQ
  | EOF
  | ELSE
  | DIV
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val parseExpr: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.expr)
