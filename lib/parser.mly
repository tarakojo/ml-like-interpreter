%{
open Syntax
%}

%token <int> INT 
%token <string> LOWER_IDENT
%token ADD SUB MUL DIV  
%token EQ LT 
%token TRUE FALSE
%token IF THEN ELSE 
%token LET REC IN
%token FUN RIGHT_ARROW
%token LPAREN RPAREN
%token DOUBLE_SEMI
%token EOF

%nonassoc EQ LT
%left ADD SUB 
%left MUL DIV

%start<Syntax.expr> parseExpr
%start<Syntax.command> parseCommand


%% 

parseExpr : 
    expression EOF {$1}
;
parseCommand : 
    expression DOUBLE_SEMI { CExp ($1) }
|   LET LOWER_IDENT EQ expression DOUBLE_SEMI { CLet($2, $4) }
|   LET REC LOWER_IDENT LOWER_IDENT EQ expression DOUBLE_SEMI { CRLet($3, $4, $6) }
;
expression : 
|   IF expression THEN expression ELSE expression { EIf ($2, $4, $6) }
|   LET LOWER_IDENT EQ expression IN expression { ELet($2, $4, $6) }
|   LET REC LOWER_IDENT LOWER_IDENT EQ expression IN expression { ERLet($3, $4, $6, $8) }
|   FUN LOWER_IDENT RIGHT_ARROW expression { EAbs($2, $4) }
|   expr1 { $1 }
;
expr1 : 
|   expr1 ADD expr1 { EBin (OpAdd, $1, $3) }
|   expr1 SUB expr1 { EBin (OpSub, $1, $3) }
|   expr1 MUL expr1 { EBin (OpMul, $1, $3) }
|   expr1 DIV expr1 { EBin (OpDiv, $1, $3) }
|   expr1 EQ expr1 { EEq ($1, $3) }
|   expr1 LT expr1 { ELt ($1, $3) }
|   expr2 { $1 }
;
expr2 : 
    expr2 expr3 { EApp($1, $2) }
|   expr3 { $1 }
;
expr3 : 
    LPAREN expression RPAREN { $2 }
|   INT { EValue(VInt($1)) }
|   TRUE { EValue(VBool(true)) }
|   FALSE { EValue(VBool(false)) }
|   LOWER_IDENT { EVar ($1) }
;