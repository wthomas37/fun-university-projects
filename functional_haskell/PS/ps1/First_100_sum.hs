-- | A module showing three techniques of summing [0,...,99] under constraints

module First_100_sum where

-- | Given two integers d, n, return True if d evenly divides n
divisibleBy :: Integer -> Integer -> Bool
divisibleBy d n
    | n `mod` d == 0 = True 
    | otherwise = False

-- | Given a list of predicates ps and a value x, return True iff p x is 
--   True for every p in ps.

allp :: [(Integer -> Bool)] -> Integer -> Bool
allp [] x = False
allp (p:[]) x
    | p x = True 
    | otherwise = False
allp (p:ps) x
    | p x = allp ps x
    | otherwise = False 

-- | Combine filter with allp such that if a value fails the allp test
--   it is discarded

filterAll :: [(Integer -> Bool)] -> [Integer] -> [Integer]
filterAll ps (n:ns)
    | allp ps n = (n:(filterAll ps ns))
    | otherwise = filterAll ps ns 
filterAll ps [] = []

result = sum 
        . take 100
        . filter (divisibleBy 2)
        . filter (divisibleBy 3)
        . filter (not . divisibleBy 4)
        . filter (not . divisibleBy 9)
        $ [0..]

result2 = sum
        . take 100
        . filter (allp [divisibleBy 2,
                        divisibleBy 3,
                        not . divisibleBy 4,
                        not . divisibleBy 9])
        $ [0..]

result3 = sum
        . take 100
        . filterAll [divisibleBy 2,
                     divisibleBy 3, 
                     not . divisibleBy 4,
                     not . divisibleBy 9]
        $ [0..]

