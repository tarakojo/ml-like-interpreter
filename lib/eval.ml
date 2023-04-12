open Syntax 

exception Eval_error 

let rec eval env e = 
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
    let v1 = getint (eval env e1) in 
    let v2 = getint (eval env e2) in (
    match op with
    | OpAdd -> VInt (v1 + v2)
    | OpSub -> VInt (v1 - v2)
    | OpMul -> VInt (v1 * v2)
    | OpDiv -> VInt (v1 / v2))
  | EEq (e1, e2) -> VBool(eval env e1 = eval env e2)
  | ELt (e1, e2) -> 
    let v1 = getint (eval env e1) in 
    let v2 = getint (eval env e2) in 
    if v1 < v2 then VBool true
    else VBool false 
  | EIf (cond, et, ef) -> 
    let c = getbool (eval env cond) in 
    if c then eval env et else eval env ef 
  | EVar x -> 
      List.assoc x env 
  | ELet (x, e1, e2) -> 
      let v1 = eval env e1 in 
      eval ((x, v1) :: env) e2
  | ERLet (f, x, e1, e2) -> 
      eval ((f, VRFun(f,x,e1,env)) :: env) e2
  | EAbs (x, e) -> VFun (x, e, env)
  | EApp (e1, e2) -> 
      match (eval env e1, eval env e2) with
      | (VFun (x, e, env'), v) -> 
          eval ((x, v) :: env') e 
      | (VRFun (f, x, e, env'), v) -> 
          eval ((x, v) :: (f, VRFun(f,x,e1,env')) :: env') e
      | _ -> raise Eval_error  
        