type name = string

and 'env value =
  | VUnit 
  | VInt of int
  | VBool of bool
  | VFun of name * 'env expr * 'env
  | VRFun of int * (name * name * 'env expr) list * 'env
  | VList of 'env value list
  | VTuple of 'env value list

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

and 'env expr =
  | EValue of 'env value
  | EUnary of unary_op * 'env expr
  | EBin of binary_op * 'env expr * 'env expr
  | ENil
  | ETuple of 'env expr list
  | EIf of 'env expr * 'env expr * 'env expr
  | EVar of name
  | ELet of name * 'env expr * 'env expr
  | ERLet of (name * name * 'env expr) list * 'env expr
  | EAbs of name * 'env expr
  | EApp of 'env expr * 'env expr
  | EMatch of 'env expr * (pattern * 'env expr) list

and pattern =
  | PVar of name
  | PInt of int
  | PBool of bool
  | PNil
  | PCons of pattern * pattern
  | PTuple of pattern list

type 'env command =
  | CExp of 'env expr
  | CLet of name * 'env expr
  | CRLet of (name * name * 'env expr) list
  | CTest of 'env expr * 'env value

type ty =
  | TyUnit 
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyTuple of ty list
  | TyList of ty
  | TyVar of name
