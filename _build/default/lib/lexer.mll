
{
open Parser 
}

rule tokenize = parse 
    [' ''\t''\r''\n'] {tokenize lexbuf}
|   ('0' | ['1'-'9']['0'-'9']*) {INT(int_of_string(Lexing.lexeme lexbuf))}
|   '+' {ADD}
|   '-' {SUB}
|   '*' {MUL}
|   '/' {DIV}
|   "true" {TRUE}
|   "false" {FALSE}
|   '=' {EQ}
|   '<' {LT}
|   "if" {IF}
|   "then" {THEN}
|   "else" {ELSE}
|   eof {EOF}
|   _ {failwith "invalid token"}