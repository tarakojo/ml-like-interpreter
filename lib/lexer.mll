
{
open Parser 
}

let natural = '0' | ['1'-'9']['0'-'9']*
let lowerIdent = ['a'-'z''_']['a'-'z''A'-'Z''0'-'9''_''\'']*

rule tokenize = parse 
    [' ''\t''\r''\n'] {tokenize lexbuf}
|   natural {INT(int_of_string(Lexing.lexeme lexbuf))}
|   "true" {TRUE}
|   "false" {FALSE}
|   '+' {ADD}
|   '-' {SUB}
|   '*' {MUL}
|   '/' {DIV}
|   '=' {EQ}
|   '<' {LT}
|   "if" {IF}
|   "then" {THEN}
|   "else" {ELSE}
|   "let" {LET}
|   "rec" {REC}
|   "in" {IN}
|   "fun" {FUN}
|   "->" {RIGHT_ARROW}
|   "(" {LPAREN}
|   ")" {RPAREN}
|   ";;" {DOUBLE_SEMI}
|   lowerIdent { LOWER_IDENT(Lexing.lexeme lexbuf) }
|   eof {EOF}
|   _ {failwith "invalid token"}