let parse_command tokenize lexbuf =
  try Parser_.parse_command tokenize lexbuf
  with Parser_.Error -> Exception.error "syntax error"
