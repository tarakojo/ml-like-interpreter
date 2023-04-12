type token =
  | INT of (int)
  | ID of (string)
  | LET
  | IN
  | EQ
  | PLUS
  | LPAR
  | RPAR
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.expr
