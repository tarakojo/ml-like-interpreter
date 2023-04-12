

let rec insert : int -> int list -> int list list = fun n -> function  
  | [] -> [[n] ]
  | h :: t -> (n :: h :: t) :: (List.map (fun l' -> h :: l') (insert n t))


let rec perm : int list -> int list list  = function
| [] -> [[]]
| h :: t ->
   let tperm = perm t in List.map (insert h) tperm |> List.concat


