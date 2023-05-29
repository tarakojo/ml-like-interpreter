let parse_command tokenize lexbuf =
  let tokenize lexbuf = try tokenize lexbuf with _ -> raise Parser_.Error in
  try Parser_.parse_command tokenize lexbuf
  with Parser_.Error -> Exception.error "syntax error"
