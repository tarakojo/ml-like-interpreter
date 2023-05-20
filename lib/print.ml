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
  | VUnit -> "()"
  | VInt x -> string_of_int x
  | VBool x -> string_of_bool x
  | VFun _ -> "function"
  | VRFun _ -> "rec function"
  | VList lis -> concat "[" ";" "]" (List.map string_of_value lis)
  | VTuple lis -> concat "(" "," ")" (List.map string_of_value lis)

let print_value v = v |> string_of_value |> print_string

let rec string_of_type = function
  | TyUnit -> "unit"
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFun (x, y) -> "(" ^ string_of_type x ^ " -> " ^ string_of_type y ^ ")"
  | TyTuple xs -> concat "(" " * " ")" (List.map string_of_type xs)
  | TyList x -> string_of_type x ^ " list"
  | TyVar x -> x

let print_type t = t |> string_of_type |> print_string
