type token =
  | INT of (int)
  | ADD
  | SUB
  | MUL
  | DIV
  | EQ
  | LT
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | EOF

val parseExpr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.expr
