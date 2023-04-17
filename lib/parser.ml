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

open Parsing;;
let _ = parse_error;;
# 2 "lib/parser.mly"
open Syntax
# 21 "lib/parser.ml"
let yytransl_const = [|
  258 (* ADD *);
  259 (* SUB *);
  260 (* MUL *);
  261 (* DIV *);
  262 (* EQ *);
  263 (* LT *);
  264 (* TRUE *);
  265 (* FALSE *);
  266 (* IF *);
  267 (* THEN *);
  268 (* ELSE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\003\000\003\000\003\000\003\000\
\003\000\003\000\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\003\000\004\000\000\000\012\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\000\000\007\000\008\000\000\000\000\000\000\000\
\000\000\000\000"

let yydgoto = "\002\000\
\007\000\008\000"

let yysindex = "\255\255\
\037\255\000\000\000\000\000\000\000\000\037\255\000\000\020\000\
\030\255\037\255\037\255\037\255\037\255\037\255\037\255\000\000\
\037\255\006\255\006\255\000\000\000\000\052\255\052\255\019\255\
\037\255\046\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\003\000\000\000\000\000\005\000\007\000\000\000\
\000\000\009\000"

let yygindex = "\000\000\
\000\000\002\000"

let yytablesize = 283
let yytable = "\001\000\
\005\000\000\000\006\000\000\000\009\000\000\000\010\000\009\000\
\011\000\012\000\013\000\018\000\019\000\020\000\021\000\022\000\
\023\000\000\000\024\000\016\000\010\000\011\000\012\000\013\000\
\014\000\015\000\026\000\000\000\000\000\000\000\025\000\010\000\
\011\000\012\000\013\000\014\000\015\000\003\000\000\000\000\000\
\017\000\000\000\000\000\000\000\004\000\005\000\006\000\010\000\
\011\000\012\000\013\000\014\000\015\000\010\000\011\000\012\000\
\013\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\005\000\005\000\006\000\006\000\005\000\005\000\
\006\000\006\000\000\000\005\000\005\000\006\000\006\000\009\000\
\009\000\010\000\010\000\011\000\011\000\010\000\011\000\012\000\
\013\000\014\000\015\000"

let yycheck = "\001\000\
\000\000\255\255\000\000\255\255\000\000\255\255\000\000\006\000\
\000\000\004\001\005\001\010\000\011\000\012\000\013\000\014\000\
\015\000\255\255\017\000\000\000\002\001\003\001\004\001\005\001\
\006\001\007\001\025\000\255\255\255\255\255\255\012\001\002\001\
\003\001\004\001\005\001\006\001\007\001\001\001\255\255\255\255\
\011\001\255\255\255\255\255\255\008\001\009\001\010\001\002\001\
\003\001\004\001\005\001\006\001\007\001\002\001\003\001\004\001\
\005\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\002\001\003\001\006\001\007\001\
\006\001\007\001\255\255\011\001\012\001\011\001\012\001\011\001\
\012\001\011\001\012\001\011\001\012\001\002\001\003\001\004\001\
\005\001\006\001\007\001"

let yynames_const = "\
  ADD\000\
  SUB\000\
  MUL\000\
  DIV\000\
  EQ\000\
  LT\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 23 "lib/parser.mly"
                   (_1)
# 176 "lib/parser.ml"
               : Syntax.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 26 "lib/parser.mly"
        ( EValue(VInt(_1)) )
# 183 "lib/parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 27 "lib/parser.mly"
         ( EValue(VBool(true)) )
# 189 "lib/parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 28 "lib/parser.mly"
          ( EValue(VBool(false)) )
# 195 "lib/parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 29 "lib/parser.mly"
                              ( EBin (OpAdd, _1, _3) )
# 203 "lib/parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 30 "lib/parser.mly"
                              ( EBin (OpSub, _1, _3) )
# 211 "lib/parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 31 "lib/parser.mly"
                              ( EBin (OpMul, _1, _3) )
# 219 "lib/parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 32 "lib/parser.mly"
                              ( EBin (OpDiv, _1, _3) )
# 227 "lib/parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 33 "lib/parser.mly"
                             ( EEq (_1, _3) )
# 235 "lib/parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 34 "lib/parser.mly"
                             ( ELt (_1, _3) )
# 243 "lib/parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 35 "lib/parser.mly"
                                                  ( EIf (_2, _4, _6) )
# 252 "lib/parser.ml"
               : 'expression))
(* Entry parseExpr *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let parseExpr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.expr)
