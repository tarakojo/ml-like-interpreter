
{
open Parser_ 

let buffer_from_stdin() = Lexing.from_channel stdin ~with_positions:false
let buffer_from_string s = Lexing.from_string ~with_positions:false s 
let buffer_from_file path = 
    let lexbuf = Lexing.from_channel (open_in path) in 
    Lexing.set_position lexbuf { 
        pos_fname = "this field is ignored";
        pos_lnum = 1;
        pos_bol = 0;
        pos_cnum = 0
    };
    Lexing.set_filename lexbuf path; 
    lexbuf

}

let natural = '0' | ['1'-'9']['0'-'9']*
let lowerIdent = ['a'-'z''_']['a'-'z''A'-'Z''0'-'9''_''\'']*

rule tokenize = parse 
    [' ''\t''\r'] {tokenize lexbuf}
|   '\n' { Lexing.new_line lexbuf; tokenize lexbuf }
|   natural {INT(int_of_string(Lexing.lexeme lexbuf))}
|   "==>" { TEST }
|   "true" {BOOL(true)}
|   "false" {BOOL(false)}
|   '+' {ADD}
|   '-' {SUB}
|   '*' {MUL}
|   '/' {DIV}
|   '%' {MOD}
|   '=' {EQ}
|   "<>" {NE}
|   '<' {LT}
|   "<=" {LE}
|   '>' {GT}
|   ">=" {GE}
|   "&&" {BAND}
|   "||" {BOR}
|   "if" {IF}
|   "then" {THEN}
|   "else" {ELSE}
|   "let" {LET}
|   "rec" {REC}
|   "and" {AND}
|   "in" {IN}
|   "fun" {FUN}
|   "->" {RIGHT_ARROW}
|   "match" {MATCH}
|   "with" {WITH}
|   "|" {PIPE}
|   "end" {END}
|   "(" {LPAREN}
|   ")" {RPAREN}
|   "[" {LSQUARE}
|   "]" {RSQUARE}
|   "," {COMMA}
|   "::" {DOUBLE_COLON}
|   ";" {SEMI}
|   ";;" {DOUBLE_SEMI}
|   lowerIdent { LOWER_IDENT(Lexing.lexeme lexbuf) }
|   eof {EOF}
|   _ {failwith "invalid token"}