
type value = 
    VInt of int 
  | VBool of bool
  | VFun of string * expr * (string * value) list
  | VRFun of string * string * expr * (string * value) list

and binOp = OpAdd | OpSub | OpMul | OpDiv 

and expr = EValue of value  
          | EBin of binOp * expr * expr 
          | EEq of expr * expr 
          | ELt of expr * expr 
          | EIf of expr * expr * expr 
          | EVar of string
          | ELet of string * expr * expr 
          | ERLet of string * string * expr * expr 
          | EAbs of string * expr 
          | EApp of expr * expr 

type command = CExp of expr
             | CLet of string * expr
             | CRLet of string * string * expr 
             