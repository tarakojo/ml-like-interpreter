%{
open Syntax
%}

%token <int> INT 
%token <string> LOWER_IDENT
%token ADD SUB MUL DIV  
%token EQ LT 
%token TRUE FALSE
%token IF THEN ELSE 
%token LET IN
%token LPAREN RPAREN
%token DOUBLE_SEMI
%token EOF

%nonassoc IN
%nonassoc ELSE
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
;
expression : 
    INT { EValue(VInt($1)) }
|   TRUE { EValue(VBool(true)) }
|   FALSE { EValue(VBool(false)) }
|   expression ADD expression { EBin (OpAdd, $1, $3) }
|   expression SUB expression { EBin (OpSub, $1, $3) }
|   expression MUL expression { EBin (OpMul, $1, $3) }
|   expression DIV expression { EBin (OpDiv, $1, $3) }
|   expression EQ expression { EEq ($1, $3) }
|   expression LT expression { ELt ($1, $3) }
|   IF expression THEN expression ELSE expression { EIf ($2, $4, $6) }
|   LPAREN expression RPAREN { $2 }
|   LOWER_IDENT { EVar ($1) }
|   LET LOWER_IDENT EQ expression IN expression { ELet($2, $4, $6) }
;
