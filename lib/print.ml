open Syntax

let concat head delim tail lis =
  let buf = Buffer.create 100 in
  let rec loop = function
    | [] -> ()
    | h :: [] -> Buffer.add_string buf h
    | h :: t ->
        Buffer.add_string buf h;
        Buffer.add_string buf delim;
        loop t
  in
  Buffer.add_string buf head;
  loop lis;
  Buffer.add_string buf tail;
  Buffer.contents buf

let rec string_of_value = function
  | VInt x -> string_of_int x
  | VBool x -> string_of_bool x
  | VFun _ -> "function"
  | VRFun _ -> "rec function"
  | VList lis -> concat "[" ";" "]" (List.map string_of_value lis)
  | VTuple lis -> concat "(" "," ")" (List.map string_of_value lis)

let print_value v = v |> string_of_value |> print_string

