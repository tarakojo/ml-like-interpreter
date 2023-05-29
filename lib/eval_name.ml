open Syntax
open Exception

type env = Env of binding list
and expr = env Syntax.expr
and value = env Syntax.value
and binding = name * thunk
and thunk = env * expr

let add_binding (Env lis) (b : binding) = Env (b :: lis)

let add_recfunction (Env env) fs =
  let lis : binding list =
    List.mapi
      (fun i (f, _, _) -> (f, (Env env, EValue (VRFun (i, fs, Env env)))))
      fs
  in
  Env (lis @ env)

(* thunk -> value *)
let rec eval (env : env) (e : expr) : value =
  match e with
  | EValue v -> v
  | EUnary (op, e1) -> (
      match op with
      | OpInv ->
          let v1 = Eval.get_int (eval env e1) in
          Eval.vint (-v1))
  | EBin (op, e1, e2) -> (
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      match op with
      | OpAdd -> Eval.int_bin ( + ) v1 v2
      | OpSub -> Eval.int_bin ( - ) v1 v2
      | OpMul -> Eval.int_bin ( * ) v1 v2
      | OpDiv -> Eval.int_bin ( / ) v1 v2
      | OpMod -> Eval.int_bin ( mod ) v1 v2
      | OpLT -> Eval.intcmp_bin ( < ) v1 v2
      | OpLE -> Eval.intcmp_bin ( <= ) v1 v2
      | OpGT -> Eval.intcmp_bin ( > ) v1 v2
      | OpGE -> Eval.intcmp_bin ( >= ) v1 v2
      | OpAnd -> Eval.bool_bin ( && ) v1 v2
      | OpOr -> Eval.bool_bin ( || ) v1 v2
      | OpCons ->
          let lis = Eval.get_list v2 in
          VList (v1 :: lis)
      | OpEQ -> Eval.vbool (v1 = v2)
      | OpNE -> Eval.vbool (v1 <> v2))
  | ENil -> VList []
  | ETuple t -> VTuple (List.map (eval env) t)
  | EIf (cond, et, ef) ->
      let c = Eval.get_bool (eval env cond) in
      if c then eval env et else eval env ef
  | EVar x -> (
      match env with
      | Env env ->
          let env', e' =
            try List.assoc x env
            with _ -> eval_error ("Unbound value " ^ x)
          in
          eval env' e')
  | ELet (x, e1, e2) -> eval (add_binding env (x, (env, e1))) e2
  | ERLet (fs, e') -> eval (add_recfunction env fs) e'
  | EAbs (x, e) -> VFun (x, e, env)
  | EApp (e1, e2) -> (
      let fv = eval env e1 in
      match fv with
      | VFun (x, e', env') -> eval (add_binding env' (x, (env, e2))) e'
      | VRFun (i, fs, env') ->
          let _, x, e' = List.nth fs i in
          let env'' = add_binding (add_recfunction env' fs) (x, (env, e2)) in
          eval env'' e'
      | _ -> eval_error "application to a non-function")
  | EMatch _ -> assert false
