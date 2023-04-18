open Syntax 

exception Eval_error 
type env = (string * value) list


let rec eval (env : env) (e : expr) : value= 
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
    let v2 = getint (eval env e2) in 
    (match op with
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
  | ECons (h, t) ->
    let hv = eval env h in  
    (match eval env t with
    | VList l -> VList(hv :: l) 
    | _ -> raise Eval_error )
  | EList l -> 
    VList (List.map (eval env) l) 
  | ETuple t -> 
    VTuple (List.map (eval env) t)   
  | EIf (cond, et, ef) -> 
    let c = getbool (eval env cond) in 
    if c then eval env et else eval env ef 
  | EVar x -> 
    (try 
        List.assoc x env
      with 
      | _ -> print_endline (x ^ " is not defined.") ; raise Eval_error 
    )
  | ELet (x, e1, e2) -> 
      let v1 = eval env e1 in 
      eval ((x, v1) :: env) e2
  | ERLet (f, x, e1, e2) -> 
      eval ((f, VRFun(f,x,e1,env)) :: env) e2
  | EAbs (x, e) -> VFun (x, e, env)
  | EApp (e1, e2) -> 
      let fv = eval env e1 in 
      let v = eval env e2 in 
     ( match fv with
      | VFun (x, e', env') -> 
          eval ((x, v) :: env') e' 
      | VRFun (f, x, e', env') -> 
          eval ( (x, v) :: (f, VRFun(f,x,e',env')) :: env') e'
      | _ -> raise Eval_error  )
  | EMatch (e', branches) -> evalMatch env (eval env e') branches
  

and evalMatch env v = function
| [] -> raise Eval_error
| (pat, e) :: t -> 
    match checkPattern pat v with
    | None -> evalMatch env v t 
    | Some env' -> eval (env' @ env) e  

and checkPattern pat v = 
  match (pat, v) with
  | (PVar x, _) -> Some [(x, v)]
  | (PInt x, VInt y) when x = y -> Some []
  | (PBool x, VBool y) when x = y -> Some []
  | (PCons (pat1, pat2), VList (h :: t)) -> (
      match (checkPattern pat1 h, checkPattern pat2 (VList t)) with
      | (Some env1, Some env2) -> Some (env1 @ env2)
      | _ -> None)
  | (PList pats, VList vs) 
  | (PTuple pats, VTuple vs) ->  
    let envs = List.map2 checkPattern pats vs in 
    if List.exists Option.is_none envs then None 
    else 
      let envs = List.map Option.get envs in 
      Some (List.flatten envs) 
  | _ -> None
    