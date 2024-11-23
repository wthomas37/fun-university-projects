{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use list comprehension" #-}

cartesianProduct :: {--forall a b.--} [a] -> [b] -> [(a,b)] 
cartesianProduct as bs = 
    concatMap (\a -> 
        map (\b ->
            (a, b)
        ) bs
    ) as





cartesianProduct3 ::
    forall a b c. [a] -> [b] -> [c] -> [(a, b, c)]
cartesianProduct3 as bs cs =
    concatMap (\a ->
        concatMap (\b ->
            concatMap (\c ->
                [(a, b, c)]
            ) cs
        ) bs
    ) as 

lessThanPairs :: forall a. Ord a => [a] -> [a] -> [(a, a)]
lessThanPairs as bs = 
    [(a, b) | a <- as, b <- bs ] -- , a < b]

-- lessThanPairs as bs = 
--     concatMap (\a ->
--         concatMap (\b ->
--             if a < b 
--                 then [(a,b)]
--                 else []
--             ) bs
--         ) as

