
open Syntax

let printValue = function
    | VInt x -> print_int x 
    | VBool b -> print_string (if b then "true" else "false")
    | VFun _ -> print_string "function"
    | VRFun _ -> print_string "rec function"


let rec printExpr : expr -> unit = function 
    | EValue v -> 
        print_string "EValue("; 
        (match v with
        | VInt x -> print_string "VInt(" ; print_int x ; print_string "))"
        | VBool x -> print_string ("VBool(" ^ (if x then "true" else "false")); print_string "))"
        | VFun _ -> print_string "VFun"
        | VRFun _ -> print_string "VRFun")
    | EBin (op, e1, e2)  -> 
        print_string "EBin(";
        (match op with
        | OpAdd -> print_string "+,"
        | OpSub -> print_string "-,"
        | OpMul -> print_string "*,"
        | OpDiv -> print_string "/,");
        printExpr e1; print_string ","; printExpr e2; print_string ")"
    | EEq (e1, e2) -> 
        print_string "EEq(";
        printExpr e1 ; 
        print_string ",";
        printExpr e2; 
        print_string ")"
    | ELt (e1, e2) -> 
        print_string "ELt(";
        printExpr e1 ; 
        print_string ",";
        printExpr e2; 
        print_string ")"
    | EIf (cond, e1, e2) -> 
        print_string "EIf(";
        printExpr cond; 
        print_string ",";
        printExpr e1; 
        print_string ",";
        printExpr e2; 
        print_string ")"
    | EVar x -> 
        print_string "EVar(";
        print_string x ;
        print_string ")"
    
    | ELet (x, e1, e2) -> 
        print_string ("ELet("^x^",");
        printExpr e1; 
        print_string ",";
        printExpr e2 ;
        print_string ")"
    
    | ERLet (f, x, e1, e2) -> 
        print_string ("ERLet("^f^","^x^",");
        printExpr e1;
        print_string ",";
        printExpr e2 ; 
        print_string ")"
    
    | EAbs (x, e) -> 
        print_string ("EAbs("^x^",");
        printExpr e ;
        print_string ")"

    | EApp (e1, e2) -> 
        print_string ("EApp(");
        printExpr e1; 
        print_string ",";
        printExpr e2;
        print_string ")"