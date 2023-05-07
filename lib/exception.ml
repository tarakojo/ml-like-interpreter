
exception Error of string

let error msg = raise (Error msg)
let syntax_error msg = raise (Error ("syntax error : "^msg))
let eval_error msg = raise (Error ("eval error : "^msg))
let type_error msg = raise (Error ("type error : "^msg))