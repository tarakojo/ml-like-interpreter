

let nth_prime is_prime n = 
    let rec f m i = 
        if is_prime i then 
            if m= i then i 
            else f (m+1) (i+1)
        else f m (i+1)
    in 
    f 0 2 
        


let is_prime x =
    let rec is_divisible_from_2_to n =
    (n > 1) && ((x mod n = 0) || is_divisible_from_2_to (n-1))
    in not (is_divisible_from_2_to (x-1))

let is_prime1 x = 
    let rec f y = 
        if y = x then true 
        else not (x mod y = 0) && f (y + 1) in 
    if x = 1 then false 
    else f 2  

let is_prime2 x = 
    let rec f y = 
        if y * y > x then true 
        else not (x mod y = 0) && f (y + 1) in 
    if x = 1 then false 
    else f 2    

    