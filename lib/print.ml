
open Syntax

let printValue = function
    | VInt x -> print_int x 
    | VBool b -> print_string (if b then "true" else "false")