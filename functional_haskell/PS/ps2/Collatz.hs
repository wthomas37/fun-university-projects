-- | Just the first exercise of the homework

module Collatz where 

collatz :: Int -> Int 
collatz n
    | n == 1 = 1 
    | even n = 1 + collatz (div n 2)
    | otherwise = 1 + collatz (3*n + 1)

collatz' :: Int -> Int 
collatz' n =
    if n == 1 then 1 
    else if even n then 1 + collatz' (div n 2) 
    else 1 + collatz' (3 * n + 1)