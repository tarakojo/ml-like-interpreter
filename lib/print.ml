
open Syntax

let printValue = function
    | VInt x -> print_int x 
    | VBool b -> print_string (if b then "true" else "false")
    | VFun _ -> print_string "function"
    | VRFun _ -> print_string "rec function"
