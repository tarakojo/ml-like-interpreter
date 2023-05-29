type name = string
and binding = BRec of expr * (name * expr) list * env | BNonRec of thunk
and thunk = env * expr
and env = (name * binding) list

and 't value' =
  | VUnit
  | VInt of int
  | VBool of bool
  | VFun of name * expr * env
  | VNil
  | VCons of 't * 't
  | VTuple of 't list

and value = thunk value'
and strict_value = SV of strict_value value'

(**)
and unary_op = OpInv | OpNot

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
  | OpEQ
  | OpNE

and expr =
  | EUnit
  | EInt of int
  | EBool of bool
  | EUnary of unary_op * expr
  | EBin of binary_op * expr * expr
  | EAnd of expr * expr
  | EOr of expr * expr
  | ENil
  | ECons of expr * expr
  | ETuple of expr list
  | EIf of expr * expr * expr
  | EVar of name
  | ELet of name * expr * expr
  | ERLet of (name * expr) list * expr
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
  | CRLet of (name * expr) list

type ty =
  | TyUnit
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyTuple of ty list
  | TyList of ty
  | TyVar of name
