
(*課題1*)

type nat = Z | S of nat

let rec add x y = 
    match x with
    | Z -> y 
    | S x' -> add x' (S y) 

let rec sub x y = 
    match (x, y) with
    | (_, Z) -> x 
    | (Z, _) -> Z 
    | (S x', S y') -> sub x' y' 

let rec mul x y = 
    match x with
    | Z -> Z 
    | S x' -> add y (mul x' y)

let rec pow x n = 
    match n with
    | Z -> S Z 
    | S n' -> mul x (pow x n')

let rec i2n i = 
  if i = 0 then Z 
  else if i > 0 then S (i2n (i-1)) 
  else failwith "error"

let rec n2i n = 
  match n with
  | Z -> 0 
  | S n' -> 1 + n2i n' 


(*課題2*)
type bnat = 
  | Z 
  | B0 of bnat
  | B1 of bnat 

  (*
  0 = Z 
  1 = B1 Z 
  2 = B0 (B1 Z) 
  3 = B1 (B1 Z) 
  4 = B0 (B0 (B1 Z))    
  *)


let halfAdd x y = (x <> y, x && y) 
let fullAdd x y z = 
  let (sXY, cXY) = halfAdd x y in 
  let (sXYZ, cXYZ) = halfAdd sXY z  in 
  (sXYZ, cXY || cXYZ) 
let mlb = function
| Z -> false
| B0 _ -> false
| B1 _ -> true 

let shift = function
| Z -> Z 
| B0 x 
| B1 x -> x 

let b_add m n = 
  let rec loop c x y = 
    if not c && x = Z && y = Z then Z
    else   
      let xmlb = mlb x in 
      let ymlb = mlb y in 
      let (s, nextc) =  fullAdd c xmlb ymlb in 
      let t =  loop nextc (shift x) (shift y) in 
      if s then B1 t 
      else B0 t 
    in 
  loop false m n 


let rec incr z = 
  match z  with
  | Z -> B1 Z 
  | B0 z' -> B1 z' 
  | B1 z' -> B0 (incr z') 

let rec decr z = 
  match z with
  | Z -> Z 
  | B0 z' -> B1 (decr z')
  | B1 Z -> Z 
  | B1 z' -> B0 z' 
  
let rec add_bin x y =
  if x = Z then y else add_bin (decr x) (incr y)

let rec sub_bin x y = 
  if y = Z then Z 
  else sub_bin (decr x) (decr y)

let rec mul_bin x y = 
  if x = Z then Z else add_bin y (mul_bin (decr x) y)

let rec pow_bin x n = 
  if n = Z then B1 Z 
  else mul_bin x (pow_bin x (decr n))

let rec i2b i = 
  if i = 0 then Z else incr (i2b (i-1))

let rec b2i b = 
  if b = Z then 0 else 1 + b2i (decr b)


(*課題3*)
type 'a tree = Leaf 
  | Node of 'a * 'a tree * 'a tree


let rec preorder t = 
  match t with
  | Leaf -> [] 
  | Node (x, l, r) -> x :: preorder l @ preorder r 

let rec postorder t = 
  match t with
  | Leaf -> [] 
  | Node (x, l, r) -> postorder l @ postorder r @ [x] 

let rec inorder t = 
  match t with
  | Leaf -> [] 
  | Node (x, l, r) -> inorder l @ [x] @ inorder r 


(*課題4*)
let levelorder tree = 
  let rec append la lb = 
    match (la, lb) with
    | (_, []) -> la 
    | ([], lb) -> lb 
    | (ha :: ta, hb :: tb) -> (ha @ hb) :: append ta tb 
  in 
  let rec f t = 
    match t with
    | Leaf -> [] 
    | Node (x, l, r) -> [x] :: append (f l) (f r)
  in 
  List.flatten (f tree)
  

(*課題5*)
type value = VInt of int | VBool of bool
type binOp = OpAdd | OpSub | OpMul | OpDiv  
type expr = EValue of value  
          | EBin of binOp * expr * expr 
          | EEq of expr * expr 
          | ELt of expr * expr 
          | EIf of expr * expr * expr 

exception Eval_error 

let rec eval e = 
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
    let v1 = getint (eval e1) in 
    let v2 = getint (eval e2) in (
    match op with
    | OpAdd -> VInt (v1 + v2)
    | OpSub -> VInt (v1 - v2)
    | OpMul -> VInt (v1 * v2)
    | OpDiv -> VInt (v1 / v2))
  | EEq (e1, e2) -> VBool(eval e1 = eval e2)
  | ELt (e1, e2) -> 
    let v1 = getint (eval e1) in 
    let v2 = getint (eval e2) in 
    if v1 < v2 then VBool true
    else VBool false 
  | EIf (cond, et, ef) -> 
    let c = getbool (eval cond) in 
    if c then eval et else eval ef 
    