open Syntax

type ty_subst = (name * ty) list
type ty_env = (name * ty) list

let rec subst ((v, t) as s) = function
  | TyUnit -> TyUnit
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyFun (x, y) -> TyFun (subst s x, subst s y)
  | TyTuple xs -> TyTuple (List.map (subst s) xs)
  | TyList x -> TyList (subst s x)
  | TyVar x when x = v -> t
  | TyVar x -> TyVar x

let rec check_noncyclic_subst v = function
  | TyVar x when x = v -> Exception.type_error "cyclic substitution"
  | TyFun (x, y) ->
      check_noncyclic_subst v x;
      check_noncyclic_subst v y
  | TyTuple xs -> List.iter (check_noncyclic_subst v) xs
  | TyList x -> check_noncyclic_subst v x
  | _ -> ()

let rec unify : Constraints.ty_constraints -> ty_subst = function
  | [] -> []
  | (x, y) :: tl when x = y -> unify tl
  | (TyFun (x1, x2), TyFun (y1, y2)) :: tl -> unify ((x1, y1) :: (x2, y2) :: tl)
  | (TyTuple xs, TyTuple ys) :: tl when List.length xs = List.length ys ->
      unify (List.combine xs ys @ tl)
  | (TyList x, TyList y) :: tl -> unify ((x, y) :: tl)
  | (TyVar x, t) :: tl | (t, TyVar x) :: tl ->
      check_noncyclic_subst x t;
      let c = List.map (fun (a, b) -> (subst (x, t) a, subst (x, t) b)) tl in
      (x, t) :: unify c
  | _ -> Exception.type_error "failed to unify"

let subst : ty_subst -> ty -> ty = List.fold_right subst

let unify c =
  let compose (v, t) s = (v, subst s t) :: s in
  List.fold_right compose (unify c) []

let infer_expr tyenv e =
  let t, c = Constraints.expr tyenv e in
  let s = unify c in
  let t' = subst s t in
  let tyenv' = List.map (fun (x, y) -> (x, subst s y)) tyenv in
  (t', tyenv')

  