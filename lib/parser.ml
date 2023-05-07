let parse_command tokenize lexbuf =
  try Parser_.parse_command tokenize lexbuf
  with Parser_.Error -> Exception.error "syntax error"

let parse_value tokenize lexbuf =
  try Parser_.parse_value tokenize lexbuf
  with Parser_.Error -> Exception.error "syntax error"
