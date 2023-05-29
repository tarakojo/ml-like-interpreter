%{
open Syntax
open Exception


module StrSet = Set.Make (String)

let rec list_to_nilcons nil cons lis = 
  match lis with
  | [] -> nil 
  | h :: t -> cons h (list_to_nilcons nil cons t)

let rec collect_vars = function 
|   PVar x -> [x]
|   PCons (h, t) -> (collect_vars h) @ (collect_vars t)
|   PTuple t -> List.concat_map collect_vars t 
|   _ -> [] 


let find_duplicated_string lis =
  let f (state : (StrSet.t, string) Either.t)  (x : string) : (StrSet.t, string) Either.t =
    match state with
    | Either.Right x -> Either.Right x
    | Either.Left set ->
        if StrSet.mem x set then Either.Right x
        else Either.Left (StrSet.add x set)
  in
  match List.fold_left f (Either.Left StrSet.empty) lis with
  | Either.Right x -> Some x 
  | Either.Left _ -> None
  
let assert_unique_names lis = 
    match find_duplicated_string lis with
    | Some x -> syntax_error (x^" is bound several times") 
    | None -> () 

let rec abs e = function
| [] -> e 
| h :: t -> EAbs(h, abs e t)

%}

%token <int> INT 
%token <bool> BOOL
%token <string> LOWER_IDENT
%token TEST
%token ADD SUB MUL DIV MOD
%token EQ NE LT LE GT GE 
%token BAND BOR BNOT
%token IF THEN ELSE 
%token LET REC AND IN
%token FUN RIGHT_ARROW
%token MATCH WITH PIPE END
%token LPAREN RPAREN LSQUARE RSQUARE
%token COMMA DOUBLE_COLON SEMI DOUBLE_SEMI 
%token EOF

%nonassoc BAND BOR 
%nonassoc EQ NE LT LE GT GE 
%right DOUBLE_COLON
%left ADD SUB 
%left MUL DIV MOD

%start<Syntax.command option> parse_command

%% 

parse_command : 
    command DOUBLE_SEMI { (Some $1) }
|   EOF { None }
;
command : 
    expression { CExp ($1) }
|   let_binding { CLet(fst $1, snd $1) }
|   letrec_binding { CRLet($1) }
;
expression : 
|   SUB expression { EUnary(OpInv, $2) }
|   BNOT expression { EUnary(OpNot, $2) }
|   IF expression THEN expression ELSE expression { EIf ($2, $4, $6) }
|   let_binding IN expression { ELet(fst $1, snd $1, $3) }
|   letrec_binding IN expression { ERLet($1, $3) }
|   FUN LOWER_IDENT RIGHT_ARROW expression { EAbs($2, $4) }
|   MATCH expression WITH option(PIPE) separated_nonempty_list(PIPE, match_branch) END { EMatch($2, $5) }
|   expr1 { $1 }
;
expr1 : 
|   expr1 ADD expr1 { EBin (OpAdd, $1, $3) }
|   expr1 SUB expr1 { EBin (OpSub, $1, $3) }
|   expr1 MUL expr1 { EBin (OpMul, $1, $3) }
|   expr1 DIV expr1 { EBin (OpDiv, $1, $3) }
|   expr1 MOD expr1 { EBin (OpMod, $1, $3) }
|   expr1 EQ expr1 { EBin (OpEQ, $1, $3) }
|   expr1 NE expr1 { EBin (OpNE, $1, $3) }
|   expr1 LT expr1 { EBin (OpLT, $1, $3) }
|   expr1 LE expr1 { EBin (OpLE, $1, $3) }
|   expr1 GT expr1 { EBin (OpGT, $1, $3) }
|   expr1 GE expr1 { EBin (OpGE, $1, $3) }
|   expr1 BAND expr1 { EAnd ($1, $3) }
|   expr1 BOR expr1 { EOr ($1, $3) }
|   expr1 DOUBLE_COLON expr1 { ECons ($1, $3) }
|   expr2 { $1 }
;
expr2 : 
    expr2 expr3 { EApp($1, $2) }
|   expr3 { $1 }
;
expr3 : 
    LPAREN RPAREN { EUnit }
|   INT { EInt($1) }
|   BOOL { EBool($1) }
|   LOWER_IDENT { EVar ($1) }
|   LPAREN expression RPAREN { $2 }
|   tuple(expression) { ETuple($1) }
|   LSQUARE separated_list(SEMI, expression) RSQUARE {
        list_to_nilcons ENil (fun h t -> ECons( h, t)) $2 
    }
;
match_branch : 
    pattern RIGHT_ARROW expression { 
        assert_unique_names (collect_vars $1); 
        ($1, $3)
    }
;
pattern : 
    LPAREN RPAREN { PUnit }
|   INT  { PInt ($1) }
|   BOOL { PBool ($1) }
|   LOWER_IDENT { PVar ($1) }
|   LPAREN pattern RPAREN { $2 }
|   pattern DOUBLE_COLON pattern { PCons ($1, $3) }
|   LSQUARE separated_list(SEMI, pattern) RSQUARE { list_to_nilcons PNil (fun h t -> PCons(h, t)) $2 }
|   tuple(pattern) { PTuple ($1) }
;

let_binding: 
    LET nonempty_list(LOWER_IDENT) EQ expression { (List.hd $2, abs $4 (List.tl $2)) }
;
letrec_binding : 
    letrec_binding_1 {
        let names = List.map (fun (x, _) -> x) $1 in 
        assert_unique_names names; 
        $1
    }
;
letrec_binding_1 : 
    LET REC letrec_binding_2 { [$3] }
|   letrec_binding_1 AND letrec_binding_2 { $3 :: $1 }
;
letrec_binding_2 : 
    nonempty_list(LOWER_IDENT) EQ expression { (List.hd $1, abs $3 (List.tl $1)) }
;



%public tuple(X):
    LPAREN X COMMA separated_nonempty_list (COMMA, X) RPAREN { $2 :: $4 }
;