type value =
  | VInt of int
  | VBool of bool
  | VFun of string * expr * (string * value) list
  | VRFun of int * (string * string * expr) list * (string * value) list
  | VList of value list
  | VTuple of value list
  
and binOp = OpAdd | OpSub | OpMul | OpDiv

and expr =
  | EValue of value
  | EBin of binOp * expr * expr
  | EEq of expr * expr
  | ELt of expr * expr
  | ECons of expr * expr
  | EList of expr list
  | ETuple of expr list
  | EIf of expr * expr * expr
  | EVar of string
  | ELet of string * expr * expr
  | ERLet of (string * string * expr) list * expr
  | EAbs of string * expr
  | EApp of expr * expr
  | EMatch of expr * (pattern * expr) list

and pattern =
  | PVar of string
  | PInt of int
  | PBool of bool
  | PCons of pattern * pattern
  | PList of pattern list
  | PTuple of pattern list

type command =
  | CExp of expr
  | CLet of string * expr
  | CRLet of (string * string * expr) list