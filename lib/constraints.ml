open Syntax

let tyvar_count = ref 0

let new_typevar () =
  tyvar_count := !tyvar_count + 1;
  TyVar ("'" ^ string_of_int !tyvar_count)

type ty_env = (name * ty) list
type ty_constraints = (ty * ty) list
type combinator = ty_env -> ty * ty_constraints

let return (t : ty) : combinator = fun _ -> (t, [])
let ( @>> ) ((x, t) : name * ty) (f : combinator) tyenv = f ((x, t) :: tyenv)

let ( --> ) (f : combinator) (t : ty) : combinator =
 fun tyenv ->
  let rt, rc = f tyenv in
  (t, (rt, t) :: rc)

let ( &&& ) (f : combinator) (g : combinator) : combinator =
 fun tyenv ->
  let _, fc = f tyenv in
  let gt, gc = g tyenv in
  (gt, fc @ gc)

let ( === ) (f : combinator) (g : combinator) : combinator =
 fun tyenv ->
  let ft, fc = f tyenv in
  let gt, gc = g tyenv in
  (gt, ((ft, gt) :: fc) @ gc)

let rec same_type (f : 'a -> combinator) (lis : 'a list) =
  match lis with
  | [] -> failwith "invalid combinator"
  | [ x ] -> f x
  | x :: xs -> f x === same_type f xs

let tuple (f : 'a -> combinator) (lis : 'a list) =
  let ts = List.init (List.length lis) (fun _ -> new_typevar ()) in
  let rec loop vlis tlis =
    match (vlis, tlis) with
    | [], [] -> return (TyTuple ts)
    | v :: vtl, t :: ttl -> f v --> t &&& loop vtl ttl
    | _ -> failwith "unreachable"
  in
  loop lis ts

let rec ( ---> ) (e : expr) (t : ty) : combinator = expr e --> t

and expr : expr -> combinator = function
  | EValue v -> value v
  | EUnary (op, e) -> ( match op with OpInv -> e ---> TyInt)
  | EBin (op, e1, e2) -> binary op e1 e2
  | ENil -> return (TyList (new_typevar ()))
  | ETuple es -> tuple expr es
  | EIf (e1, e2, e3) -> e1 ---> TyBool &&& same_type expr [ e2; e3 ]
  | EVar x -> var x
  | ELet (x, e1, e2) ->
      let xt = new_typevar () in
      e1 ---> xt &&& (x, xt) @>> expr e2
  | ERLet (lis, e) -> let_rec lis e
  | EAbs (x, e) ->
      let xt = new_typevar () in
      let rt = new_typevar () in
      (x, xt) @>> (expr e --> rt) &&& return (TyFun (xt, rt))
  | EApp (e1, e2) ->
      let a = new_typevar () in
      let b = new_typevar () in
      e1 ---> TyFun (a, b) &&& e2 ---> a &&& return b
  | _ -> failwith "unimplemented"

and value = function
  | VInt _ -> return TyInt
  | VBool _ -> return TyBool
  | VList vs ->
      let a = new_typevar () in
      same_type value vs --> a &&& return (TyList a)
  | VTuple vs -> tuple value vs
  | VFun _ | VRFun _ -> failwith "unreachable"

and binary op e1 e2 =
  match op with
  | OpAdd | OpSub | OpMul | OpDiv | OpMod -> same_type expr [ e1; e2 ] --> TyInt
  | OpLT | OpLE | OpGT | OpGE ->
      same_type expr [ e1; e2 ] --> TyInt &&& return TyBool
  | OpAnd | OpOr -> same_type expr [ e1; e2 ] --> TyBool
  | OpEQ | OpNE -> same_type expr [ e1; e2 ] &&& return TyBool
  | OpCons ->
      let a = new_typevar () in
      e1 ---> a &&& e2 ---> TyList a

and var x tyenv =
  try
    let t = List.assoc x tyenv in
    (t, [])
  with _ -> raise (Exception.error ("unbound variable " ^ x))

and let_rec lis e =
  let typevars =
    List.init (List.length lis) (fun _ ->
        ( new_typevar () (* typevar of argument*),
          new_typevar () (* typevar of result*) ))
  in
  let with_constraints : combinator -> combinator =
    List.fold_right2
      (fun (a, r) (f, _, _) c -> (f, TyFun (a, r)) @>> c)
      typevars lis
  in

  List.fold_right2
    (fun (a, r) (_, x, e') c -> (x, a) @>> with_constraints (e' ---> r) &&& c)
    typevars lis
    (with_constraints (expr e))
