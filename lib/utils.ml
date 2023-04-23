module StrSet = Set.Make (String)

let find_duplicated_string lis =
  let f (state : (StrSet.t, string) Either.t)  (x : string) : (StrSet.t, string) Either.t =
    match state with
    | Either.Right x -> Either.Right x
    | Either.Left set ->
        if StrSet.mem x set then Either.Right x
        else Either.Left (StrSet.add x set)
  in
  match List.fold_left f (Either.Left StrSet.empty) lis with
  | Either.Right x -> Some x 
  | Either.Left _ -> None
  
