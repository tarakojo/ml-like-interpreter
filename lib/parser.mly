%{
open Syntax
%}

%token <int> INT 
%token <bool> BOOL
%token <string> LOWER_IDENT
%token ADD SUB MUL DIV  
%token EQ LT 
%token IF THEN ELSE 
%token LET REC AND IN
%token FUN RIGHT_ARROW
%token MATCH WITH PIPE END
%token LPAREN RPAREN LSQUARE RSQUARE
%token COMMA DOUBLE_COLON SEMI DOUBLE_SEMI 
%token EOF

%nonassoc EQ LT
%right DOUBLE_COLON
%left ADD SUB 
%left MUL DIV

%start<Syntax.command list> parse_file
%start<Syntax.command> parse_command


%% 

parse_file : 
    parse_command parse_file { $1 :: $2 }
|   expression EOF {[CExp $1]} 
|   EOF { [] }
;
parse_command : 
    expression DOUBLE_SEMI { CExp ($1) }
|   LET LOWER_IDENT EQ expression DOUBLE_SEMI { CLet($2, $4) }
|   letrec_binding DOUBLE_SEMI { CRLet($1) }
;
expression : 
|   IF expression THEN expression ELSE expression { EIf ($2, $4, $6) }
|   LET LOWER_IDENT EQ expression IN expression { ELet($2, $4, $6) }
|   letrec_binding IN expression { ERLet($1, $3) }
|   FUN LOWER_IDENT RIGHT_ARROW expression { EAbs($2, $4) }
|   MATCH expression WITH option(PIPE) separated_list(PIPE, match_branch) END { EMatch($2, $5) }
|   expr1 { $1 }
;
expr1 : 
|   expr1 ADD expr1 { EBin (OpAdd, $1, $3) }
|   expr1 SUB expr1 { EBin (OpSub, $1, $3) }
|   expr1 MUL expr1 { EBin (OpMul, $1, $3) }
|   expr1 DIV expr1 { EBin (OpDiv, $1, $3) }
|   expr1 EQ expr1 { EEq ($1, $3) }
|   expr1 LT expr1 { ELt ($1, $3) }
|   expr1 DOUBLE_COLON expr1 { ECons ($1, $3) }
|   expr2 { $1 }
;
expr2 : 
    expr2 expr3 { EApp($1, $2) }
|   expr3 { $1 }
;
expr3 : 
    LPAREN expression RPAREN { $2 }
|   LPAREN expression COMMA separated_nonempty_list(COMMA, expression) RPAREN { ETuple($2 :: $4) }
|   LSQUARE separated_list(SEMI, expression) RSQUARE { EList($2) }
|   INT { EValue(VInt($1)) }
|   BOOL { EValue(VBool($1)) }
|   LOWER_IDENT { EVar ($1) }
;
match_branch : 
    pattern RIGHT_ARROW expression { ($1, $3) }
;
pattern : 
    INT  { PInt ($1) }
|   BOOL { PBool ($1) }
|   LOWER_IDENT { PVar ($1) }
|   LPAREN pattern RPAREN { $2 }
|   LSQUARE separated_list(SEMI, pattern) RSQUARE { PList($2) }
|   pattern DOUBLE_COLON pattern { PCons ($1, $3) }
|   LPAREN pattern COMMA separated_nonempty_list(COMMA, pattern) RPAREN { PTuple ($2 :: $4) }
;
letrec_binding : 
    letrec_binding_1 {
        let names = List.map (fun (f, _, _) -> f) $1 in 
        match Utils.find_duplicated_string names with 
        | None-> $1 
        | Some x -> 
            print_endline (x^" is bound several times"); 
            raise Parsing.Parse_error
    }
;
letrec_binding_1 : 
    LET REC letrec_binding_2 { [$3] }
|   letrec_binding_1 AND letrec_binding_2 { $3 :: $1 }
;
letrec_binding_2 : 
    LOWER_IDENT LOWER_IDENT EQ expression { ($1, $2, $4) }
;