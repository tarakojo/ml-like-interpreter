%{
open Syntax
%}

%token <int> INT 
%token ADD SUB MUL DIV  
%token EQ LT 
%token TRUE FALSE
%token IF THEN ELSE 
%token EOF

%nonassoc ELSE
%nonassoc EQ LT
%left ADD SUB 
%left MUL DIV

%start parseExpr
%type <Syntax.expr> parseExpr

%% 

parseExpr : 
    expression EOF {$1}
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
;
