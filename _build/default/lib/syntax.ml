
type value = VInt of int | VBool of bool
type binOp = OpAdd | OpSub | OpMul | OpDiv 
type expr = EValue of value  
          | EBin of binOp * expr * expr 
          | EEq of expr * expr 
          | ELt of expr * expr 
          | EIf of expr * expr * expr 
