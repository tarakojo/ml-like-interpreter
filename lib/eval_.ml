(*open Syntax
open Exception

type env = env_v
type value = value_v
type binding = value Syntax.binding

let add_recfunction (env : env) fs =
  let lis : binding list =
    List.mapi (fun i (f, _, _) -> (f, VV (VRFun (i, fs, env)))) fs
  in
  lis @ env

let unwrap (VV v) = v
let wrap v = VV v

let get_int v =
  match unwrap v with VInt x -> x | _ -> eval_error "int value is required"

let get_bool v =
  match unwrap v with VBool x -> x | _ -> eval_error "bool value is required"

let rec get_list v =
  match unwrap v with
  | VNil -> []
  | VCons (x, y) -> x :: get_list y
  | _ -> eval_error "list value is required"

let vint x = wrap (VInt x)
let vbool x = wrap (VBool x)

let calc_bin getter constructor op v1 v2 =
  constructor (op (getter v1) (getter v2))

let int_bin op x y = calc_bin get_int vint op x y
let intcmp_bin op x y = calc_bin get_int vbool op x y
let bool_bin op x y = calc_bin get_bool vbool op x y

let rec eval (env : env) (e : expr) : value =
  match e with
  | EUnit -> wrap VUnit
  | EInt x -> vint x
  | EBool x -> vbool x
  | EUnary (op, e1) -> (
      match op with
      | OpInv ->
          let v1 = get_int (eval env e1) in
          vint (-v1))
  | EBin (op, e1, e2) -> (
      let v1 = eval env e1 in
      let v2 = eval env e2 in
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
      | OpAnd -> bool_bin ( && ) v1 v2
      | OpOr -> bool_bin ( || ) v1 v2
      | OpCons -> wrap (VCons (v1, v2))
      | OpEQ -> vbool (v1 = v2)
      | OpNE -> vbool (v1 <> v2))
  | ENil -> wrap VNil
  | ETuple t -> wrap (VTuple (List.map (eval env) t))
  | EIf (cond, et, ef) ->
      let c = get_bool (eval env cond) in
      if c then eval env et else eval env ef
  | EVar x -> (
      try List.assoc x env with _ -> eval_error ("Unbound value " ^ x))
  | ELet (x, e1, e2) ->
      let v1 = eval env e1 in
      eval ((x, v1) :: env) e2
  | ERLet (fs, e') -> eval (add_recfunction env fs) e'
  | EAbs (x, e) -> wrap (VFun (x, e, env))
  | EApp (e1, e2) -> (
      let fv = eval env e1 in
      let v = eval env e2 in
      match unwrap fv with
      | VFun (x, e', env') -> eval ((x, v) :: env') e'
      | VRFun (i, fs, env') ->
          let _, x, e' = List.nth fs i in
          let env'' = (x, v) :: add_recfunction env' fs in
          eval env'' e'
      | _ -> eval_error "application to a non-function")
  | EMatch (e', branches) -> evalMatch env (eval env e') branches

and evalMatch env v = function
  | [] -> eval_error "pattern match failed"
  | (pat, e) :: t -> (
      match checkPattern pat v with
      | None -> evalMatch env v t
      | Some env' -> eval (env' @ env) e)

and checkPattern pat v =
  match (pat, unwrap v) with
  | PVar x, _ -> Some [ (x, v) ]
  | PInt x, VInt y when x = y -> Some []
  | PBool x, VBool y when x = y -> Some []
  | PNil, VNil -> Some []
  | PCons (pat1, pat2), VCons (h, t) -> (
      match (checkPattern pat1 h, checkPattern pat2 t) with
      | Some env1, Some env2 -> Some (env1 @ env2)
      | _ -> None)
  | PTuple pats, VTuple vs ->
      let envs = List.map2 checkPattern pats vs in
      if List.exists Option.is_none envs then None
      else
        let envs' = envs |> List.map Option.get |> List.flatten in
        Some envs'
  | _ -> None
*)