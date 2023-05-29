type name = string

type binding = name * value
and env = binding list

and value =
  | VUnit
  | VInt of int
  | VBool of bool
  | VFun of name * expr * env
  | VRFun of int * (name * name * expr) list * env
  | VList of value list
  | VTuple of value list

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
  | EValue of value
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
  | PInt of int
  | PBool of bool
  | PNil
  | PCons of pattern * pattern
  | PTuple of pattern list

type command =
  | CExp of expr
  | CLet of name * expr
  | CRLet of (name * name * expr) list
  | CTest of expr * value

type ty =
  | TyUnit
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyTuple of ty list
  | TyList of ty
  | TyVar of name
