open Syntax

exception TypeError of string

type ty_subst = (name * ty) list

let rec subst (v, t) = function
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyFun (x, y) -> TyFun (subst (v, t) x, subst (v, t) y)
  | TyVar x when x = v -> t
  | TyVar x -> TyVar x

let rec check_noncyclic_subst v = function
  | TyVar x when x = v -> raise (TypeError "cyclic substitution")
  | TyFun (x, y) ->
      check_noncyclic_subst v x;
      check_noncyclic_subst v y
  | _ -> ()

let rec unify = function
  | [] -> []
  | (x, y) :: tl when x = y -> unify tl
  | (TyFun (x1, x2), TyFun (y1, y2)) :: tl -> unify ((x1, y1) :: (x2, y2) :: tl)
  | (TyVar x, t) :: tl | (t, TyVar x) :: tl ->
      check_noncyclic_subst x t;
      let c = List.map (fun (a, b) -> (subst (x, t) a, subst (x, t) b)) tl in
      (x, t) :: unify c
  | _ -> raise (TypeError "")

let subst = List.fold_right subst

let unify c =
  let compose (v, t) s = (v, subst s t) :: s in
  List.fold_right compose (unify c) []


