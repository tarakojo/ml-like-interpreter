

(*課題2*)
type bnat = 
  | Z 
  | B0 of bnat
  | B1 of bnat 


let rec incr z = 
  match z  with
  | Z -> B1 Z 
  | B0 z' -> B1 z' 
  | B1 z' -> B0 (incr z') 

let rec decr z = 
  match z with
  | Z -> Z 
  | B0 z' -> B1 (decr z')
  | B1 Z -> Z 
  | B1 z' -> B0 z' 

  

let rec add_bin x y =
  let xor x y = 
    not(x = y) in 

  let mlb_bool x = 
    match x with
    | Z 
    | B0 _ -> false
    | B1 _ -> true  in 

  let shift x = 
    match x with
    | Z -> Z 
    | B0 x' 
    | B1 x' -> x'  in 

  let rec f c x y = 
    match (x, y) with
    | (Z, Z) -> if c then B1 Z else Z 
    | _ -> 
      let xb= mlb_bool x in 
      let yb = mlb_bool y in 
      let v = xor xb (xor yb c)
       
      

  

let rec sub_bin x y = 
  if y = Z then Z 
  else sub_bin (decr x) (decr y)

let rec mul_bin x y = 
  if x = Z then Z else add_bin y (mul_bin (decr x) y)

let rec pow_bin x n = 
  if n = Z then B1 Z 
  else mul_bin x (pow_bin x (decr n))

let rec i2b i = 
  if i = 0 then Z else incr (i2b (i-1))

let rec b2i b = 
  if b = Z then 0 else 1 + b2i (decr b)