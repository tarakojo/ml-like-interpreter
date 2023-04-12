open Syntax 

exception Eval_error 

let rec eval e = 
  let getint v = 
    match v with
    | VInt x -> x 
    | _ -> raise Eval_error 
  in 
  let getbool v = 
    match v with
    | VBool x -> x 
    | _ -> raise Eval_error 
  in 
  match e with
  | EValue v -> v 
  | EBin (op, e1, e2) -> 
    let v1 = getint (eval e1) in 
    let v2 = getint (eval e2) in (
    match op with
    | OpAdd -> VInt (v1 + v2)
    | OpSub -> VInt (v1 - v2)
    | OpMul -> VInt (v1 * v2)
    | OpDiv -> VInt (v1 / v2))
  | EEq (e1, e2) -> VBool(eval e1 = eval e2)
  | ELt (e1, e2) -> 
    let v1 = getint (eval e1) in 
    let v2 = getint (eval e2) in 
    if v1 < v2 then VBool true
    else VBool false 
  | EIf (cond, et, ef) -> 
    let c = getbool (eval cond) in 
    if c then eval et else eval ef 
    