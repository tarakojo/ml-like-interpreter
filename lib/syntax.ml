type name = string
and 'a binding = name * 'a
and 'a env = 'a binding list

and ('thunk, 'env) value =
  | VUnit
  | VInt of int
  | VBool of bool
  | VFun of name * expr * 'env
  | VRFun of int * (name * name * expr) list * 'env
  | VNil
  | VCons of 'thunk * 'thunk
  | VTuple of 'thunk list

and value_v = VV of (value_v, env_v) value
and env_v = value_v env
and thunk = T of expr * thunk env
and value_n = VN of (thunk, env_n) value
and env_n = thunk env

(**)
and unary_op = OpInv

and binary_op =
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  | OpLT
  | OpLE
  | OpGT
  | OpGE
  | OpAnd
  | OpOr
  | OpCons
  | OpEQ
  | OpNE

and expr =
  | EUnit
  | EInt of int
  | EBool of bool
  | EUnary of unary_op * expr
  | EBin of binary_op * expr * expr
  | ENil
  | ETuple of expr list
  | EIf of expr * expr * expr
  | EVar of name
  | ELet of name * expr * expr
  | ERLet of (name * name * expr) list * expr
  | EAbs of name * expr
  | EApp of expr * expr
  | EMatch of expr * (pattern * expr) list

and pattern =
  | PVar of name
  | PUnit
  | PInt of int
  | PBool of bool
  | PNil
  | PCons of pattern * pattern
  | PTuple of pattern list

type command =
  | CExp of expr
  | CLet of name * expr
  | CRLet of (name * name * expr) list
  | CTest of expr * value_v

type ty =
  | TyUnit
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyTuple of ty list
  | TyList of ty
  | TyVar of name
