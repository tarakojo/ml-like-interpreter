open Syntax

type ty_env = (name * ty) list
type ty_constraints = (ty * ty) list
type combinator = ty_env -> ty * ty_constraints

let tyvar_count = ref 0

let new_typevar () =
  tyvar_count := !tyvar_count + 1;
  "tyvar" ^ string_of_int !tyvar_count

let rec return (t : ty) : combinator = fun _ -> (t, [])

and ( &&& ) (f : combinator) (g : combinator) : combinator =
 fun tyenv ->
  let _, fc = f tyenv in
  let gt, gc = g tyenv in
  (gt, fc @ gc)

and ( === ) (f : combinator) (g : combinator) tyenv =
  let ft, fc = f tyenv in
  let gt, gc = g tyenv in
  (gt, ((ft, gt) :: fc) @ gc)

and same : expr list -> combinator = function
  | [] -> failwith "invalid combinator"
  | [ e ] -> expr e
  | e :: es -> expr e === same es

and ( --> ) (e : expr) (t : ty) : combinator = expr e === return t
and ( ---> ) (es : expr list) (t : ty) : combinator = same es === return t

and expr = function
  | EValue v -> value v
  | EUnary (op, e) -> unary op e
  | EBin (op, e1, e2) -> binary op e1 e2
  | EIf (e1, e2, e3) -> e1 --> TyBool &&& same [ e2; e3 ]
  | EVar x -> var x
  | ELet (x, e1, e2) -> let_e x e1 e2
  | _ -> failwith "unimplemented"

and value = function
  | VInt _ -> return TyInt
  | VBool _ -> return TyBool
  | _ -> failwith "unimplemented"

and unary op e = match op with OpInv -> e --> TyInt

and binary op e1 e2 =
  match op with
  | OpAdd | OpSub | OpMul | OpDiv | OpMod -> [ e1; e2 ] ---> TyInt
  | OpLT | OpLE | OpGT | OpGE -> [ e1; e2 ] ---> TyInt &&& return TyBool
  | OpAnd | OpOr -> [ e1; e2 ] ---> TyBool
  | OpEQ | OpNE -> same [ e1; e2 ] &&& return TyBool
  | OpCons -> failwith "unimplemented"

and var x tyenv =
  try
    let t = List.assoc x tyenv in
    (t, [])
  with _ -> raise (Exceptions.TypeError ("unbound variable " ^ x))

and let_e x e1 e2 tyenv =
  let t1, c1 = expr e1 tyenv in
  let t2, c2 = expr e2 ((x, t1) :: tyenv) in
  (t2, c1 @ c2)

