(*課題1*)

let rec sum_to n = 
  if n = 0 then 0 
  else n + sum_to (n-1)

let is_prime n = 
  let rec f m = 
      if m * m > n then true 
      else not (n mod m = 0) && f (m + 1) in 
  if n = 1 then false 
  else f 2    


let rec gcd x y = 
  if y = 0 then x 
  else gcd y (x mod y)

(*課題2*)
let rec fib_exponential n = 
  if n <= 2 then 1 else fib_exponential (n-2) + fib_exponential (n-1)
  
let rec fib_linear n = 
  let rec f i p1 p2 = 
    let x = p1 + p2 in 
    if n = i then x 
    else f (i + 1) x p1 in 
  if n <= 2 then 1 
  else f 3 1 1 

let fib_log n = 
  let mul a b = 
    let m = Array.make_matrix (Array.length a) (Array.length (b.(0))) 0 in 
    for i = 0 to Array.length m - 1 do 
      for j = 0 to Array.length (m.(0)) - 1 do 
        for k = 0 to Array.length (a.(0)) - 1 do 
          m.(i).(j) <- m.(i).(j) + a.(i).(k) * b.(k).(j) 
        done 
      done
    done;  
    m 
  in 
  let rec pow mat m = 
    if m = 0 then 
      let e = Array.make_matrix 2 2 0 in 
      e.(0).(0) <- 1; 
      e.(1).(1) <- 1; 
      e 
    else if m = 1 then mat   
    else if m mod 2 = 0 then pow (mul mat mat) (m / 2) 
    else mul mat (pow (mul mat mat) (m / 2)) 
  in 
  let fibmat = Array.make_matrix 2 2 1 in 
      fibmat.(1).(1) <- 0 ; 
  let m = pow fibmat (n - 1) in 
  m.(0).(0) 

(*課題3*)
let twice f x = f (f x)

let rec repeat n f x =
    if n = 0 then x 
    else f (repeat (n-1) f x) 


(*課題4*)
let rec fix f x = f (fix f) x 

let sum_to' n = 
    fix (fun f m -> if m = 0 then 0 else m + f (m-1)) n 

let is_prime' n = 
  let f g m = 
    if m * m > n then true 
    else not (n mod m = 0) && g (m + 1) in 
  if n = 1 then false 
  else fix f 2

let gcd' x y = 
  let f g x y = 
    if y = 0 then x 
    else g y (x mod y)
  in 
  fix f x y 

let fib' n = 
    fix (fun f m -> if m <= 2 then 1 else f (m-1) + f (m-2)) n 


(*課題5*)
let rec fold_right f lis e = 
    match lis with 
    | [] -> e
    | x :: xs -> f x (fold_right f xs e)
    

let fold_left f e lis = 
    let rec loop c l = 
      match l with 
      | [] -> c 
      | x :: xs -> loop (f c x) xs 
    in 
    loop e lis 
    

(*課題6*)
let rec append a b = 
    match a with
    | [] -> b 
    | x :: xs -> x :: append xs b 


let rec last lis = 
  match lis with
  | [] -> failwith "error"
  | [x] -> x 
  | x :: xs -> last xs 


let rec map f xs = 
    match xs with
    | [] -> [] 
    | h :: t -> f h :: map f xs 


(*課題7*)
let reverse lis = 
  let rec loop a b = 
    match a with
    | [] -> b 
    | h :: t -> loop t (h :: b) in 
  loop lis []  

  
(*課題8*)
let rec perm lis = 
  if List.length lis = 1 then [[List.hd lis]] 
  else
  let loopi i = 
    let ei = List.nth lis i in 
    let rest = List.filteri (fun j _ -> i <> j) lis in
    let p = perm rest in 
      List.map (fun x -> ei :: x) p 
  in 
  let p = List.mapi (fun i _ -> loopi i) lis in 
  let res = List.flatten p in 
  res


  