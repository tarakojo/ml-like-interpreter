open Syntax
open Exception

let load_rec (current : env) (lis : (name * expr) list) (recenv : env) : env =
  let env = List.map (fun (x, e) -> (x, BRec (e, lis, recenv))) lis in
  env @ current

let get_int v = match v with VInt x -> x | _ -> assert false
let get_bool v = match v with VBool x -> x | _ -> assert false
let vint x = VInt x
let vbool x = VBool x

let calc_bin getter constructor op v1 v2 =
  constructor (op (getter v1) (getter v2))

let int_bin op x y = calc_bin get_int vint op x y
let intcmp_bin op x y = calc_bin get_int vbool op x y
let bool_bin op x y = calc_bin get_bool vbool op x y

let rec eval ((env, e) : thunk) : value =
  match e with
  | EUnit -> VUnit
  | EInt x -> VInt x
  | EBool x -> VBool x
  | EUnary (op, e1) -> (
      match op with
      | OpInv ->
          let v1 = get_int (eval (env, e1)) in
          vint (-v1)
      | OpNot ->
          let v1 = get_bool (eval (env, e1)) in
          vbool (not v1))
  | EBin (op, e1, e2) -> (
      let v1 = eval (env, e1) in
      let v2 = eval (env, e2) in
      match op with
      | OpAdd -> int_bin ( + ) v1 v2
      | OpSub -> int_bin ( - ) v1 v2
      | OpMul -> int_bin ( * ) v1 v2
      | OpDiv -> int_bin ( / ) v1 v2
      | OpMod -> int_bin ( mod ) v1 v2
      | OpLT -> intcmp_bin ( < ) v1 v2
      | OpLE -> intcmp_bin ( <= ) v1 v2
      | OpGT -> intcmp_bin ( > ) v1 v2
      | OpGE -> intcmp_bin ( >= ) v1 v2
      | OpEQ -> vbool (v1 = v2)
      | OpNE -> vbool (v1 <> v2))
  | EAnd (e1, e2) ->
      if get_bool (eval (env, e1)) then eval (env, e2) else vbool false
  | EOr (e1, e2) ->
      if get_bool (eval (env, e1)) then vbool true else eval (env, e2)
  | ENil -> VNil
  | ECons (e1, e2) -> VCons ((env, e1), (env, e2))
  | ETuple t -> VTuple (List.map (fun e' -> (env, e')) t)
  | EIf (cond, et, ef) ->
      let c = get_bool (eval (env, cond)) in
      if c then eval (env, et) else eval (env, ef)
  | EVar x -> (
      try
        match List.assoc x env with
        | BNonRec (env', e') -> eval (env', e')
        | BRec (e', lis, env') -> eval (load_rec env lis env', e')
      with Not_found -> eval_error ("Unbound value " ^ x))
  | ELet (x, e1, e2) -> eval ((x, BNonRec (env, e1)) :: env, e2)
  | ERLet (lis, e') ->
      let bs = List.map (fun (x, ex) -> (x, BRec (ex, lis, env))) lis in
      eval (bs @ env, e')
  | EAbs (x, e') -> VFun (x, e', env)
  | EApp (e1, e2) -> (
      let fv = eval (env, e1) in
      match fv with
      | VFun (x, e', env') -> eval ((x, BNonRec (env, e2)) :: env', e')
      | _ -> assert false)
  | EMatch (e', pats) -> pattern_match (env, e') pats

and pattern_match ((env, _) as t) = function
  | [] -> eval_error "pattern match failed"
  | (pat, e) :: tl -> (
      match checkPattern pat t with
      | None -> pattern_match t tl
      | Some env' -> eval (env' @ env, e))

and checkPattern pat (t : thunk) =
  match pat with
  | PVar x -> Some [ (x, BNonRec t) ]
  | _ -> (
      match (pat, eval t) with
      | PInt x, VInt y when x = y -> Some []
      | PBool x, VBool y when x = y -> Some []
      | PNil, VNil -> Some []
      | PCons (pat1, pat2), VCons (h, tl) -> (
          match (checkPattern pat1 h, checkPattern pat2 tl) with
          | Some env1, Some env2 -> Some (env1 @ env2)
          | _ -> None)
      | PTuple pats, VTuple vs ->
          let envs = List.map2 checkPattern pats vs in
          if List.exists Option.is_none envs then None
          else
            let envs' = envs |> List.map Option.get |> List.flatten in
            Some envs'
      | _ -> None)

and strict_eval = function
  | VUnit -> SV VUnit
  | VInt x -> SV (VInt x)
  | VBool x -> SV (VBool x)
  | VFun (x, y, z) -> SV (VFun (x, y, z))
  | VNil -> SV VNil
  | VCons (t1, t2) ->
      SV (VCons (t1 |> eval |> strict_eval, t2 |> eval |> strict_eval))
  | VTuple ts -> SV (VTuple (List.map (fun t -> t |> eval |> strict_eval) ts))
