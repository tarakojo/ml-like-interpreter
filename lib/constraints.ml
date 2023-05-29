open Syntax

let tyvar_count = ref 0

let new_typevar () =
  tyvar_count := !tyvar_count + 1;
  TyVar ("'" ^ string_of_int !tyvar_count)

type ty_env = (name * ty) list
type ty_constraints = (ty * ty) list

let rec same_type = function
  | x1 :: (x2 :: _ as tl) -> (x1, x2) :: same_type tl
  | _ -> []

let rec expr (tyenv : ty_env) : expr -> ty * ty_constraints = function
  | EUnit -> (TyUnit, [])
  | EInt _ -> (TyInt, [])
  | EBool _ -> (TyBool, [])
  | EUnary (op, e) -> (
      match op with
      | OpInv ->
          let t, c = expr tyenv e in
          (TyInt, (t, TyInt) :: c))
  | EBin (op, e1, e2) -> binary tyenv op e1 e2
  | ENil -> (TyList (new_typevar ()), [])
  | ETuple es ->
      let ts, cs = es |> List.map (expr tyenv) |> List.split in
      (TyTuple ts, List.flatten cs)
  | EIf (e1, e2, e3) ->
      let t1, c1 = expr tyenv e1 in
      let t2, c2 = expr tyenv e2 in
      let t3, c3 = expr tyenv e3 in
      (t2, ((TyBool, t1) :: (t2, t3) :: c1) @ c2 @ c3)
  | EVar x -> (
      try
        let t = List.assoc x tyenv in
        (t, [])
      with _ -> raise (Exception.error ("unbound variable " ^ x)))
  | ELet (x, e1, e2) ->
      let t1, c1 = expr tyenv e1 in
      let t2, c2 = expr ((x, t1) :: tyenv) e2 in
      (t2, c1 @ c2)
  | ERLet (lis, e) -> let_rec tyenv lis e
  | EAbs (x, e) ->
      let xt = new_typevar () in
      let t, c = expr ((x, xt) :: tyenv) e in
      (TyFun (xt, t), c)
  | EApp (e1, e2) ->
      let t1, c1 = expr tyenv e1 in
      let t2, c2 = expr tyenv e2 in
      let rt = new_typevar () in
      (rt, ((t1, TyFun (t2, rt)) :: c1) @ c2)
  | EMatch (e, pats) -> pattern_match tyenv e pats

and binary tyenv op e1 e2 =
  match op with
  | OpAdd | OpSub | OpMul | OpDiv | OpMod ->
      let t1, c1 = expr tyenv e1 in
      let t2, c2 = expr tyenv e2 in
      (TyInt, ((t1, TyInt) :: (t2, TyInt) :: c1) @ c2)
  | OpLT | OpLE | OpGT | OpGE ->
      let t1, c1 = expr tyenv e1 in
      let t2, c2 = expr tyenv e2 in
      (TyBool, ((t1, TyInt) :: (t2, TyInt) :: c1) @ c2)
  | OpAnd | OpOr ->
      let t1, c1 = expr tyenv e1 in
      let t2, c2 = expr tyenv e2 in
      (TyBool, ((t1, TyBool) :: (t2, TyBool) :: c1) @ c2)
  | OpEQ | OpNE ->
      let t1, c1 = expr tyenv e1 in
      let t2, c2 = expr tyenv e2 in
      (TyBool, ((t1, t2) :: c1) @ c2)
  | OpCons ->
      let t1, c1 = expr tyenv e1 in
      let t2, c2 = expr tyenv e2 in
      (t2, ((t2, TyList t1) :: c1) @ c2)

and let_rec tyenv lis e =
  let ts =
    List.init (List.length lis) (fun _ -> (new_typevar (), new_typevar ()))
  in
  let tyenv' = List.map2 (fun (f, _, _) (a, r) -> (f, TyFun (a, r))) lis ts in
  let tyenv = tyenv' @ tyenv in
  let cs =
    List.map2
      (fun (_, x, e') (a, r) ->
        let t, c = expr ((x, a) :: tyenv) e' in
        (r, t) :: c)
      lis ts
  in
  let t, c = expr tyenv e in
  (t, List.flatten (c :: cs))

and pattern : pattern -> ty * ty_env * ty_constraints = function
  | PVar x ->
      let xt = new_typevar () in
      (xt, [ (x, xt) ], [])
  | PUnit -> (TyUnit, [], [])
  | PInt _ -> (TyInt, [], [])
  | PBool _ -> (TyBool, [], [])
  | PNil -> (TyList (new_typevar ()), [], [])
  | PCons (p1, p2) ->
      let t1, env1, c1 = pattern p1 in
      let t2, env2, c2 = pattern p2 in
      (TyList t1, env2 @ env1, ((t2, TyList t1) :: c1) @ c2)
  | PTuple ps ->
      let rs = List.map pattern ps in
      let ts, envs, cs =
        List.fold_right
          (fun (t, env, c) (ts, envs, cs) -> (t :: ts, env @ envs, c @ cs))
          rs ([], [], [])
      in
      (TyTuple ts, envs, cs)

and pattern_match tyenv e pats =
  let et, ec = expr tyenv e in
  let pts, pcs =
    pats
    |> List.map (fun (p, e') ->
           let pt, penv, pc = pattern p in
           let e't, e'c = expr (penv @ tyenv) e' in
           (e't, ((pt, et) :: pc) @ e'c))
    |> List.split
  in
  (List.hd pts, same_type pts @ List.flatten pcs @ ec)
