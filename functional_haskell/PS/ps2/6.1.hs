data Pair a b = Pair a b 

instance (ord a, ord b) => Ord (Pair a b) where 
    Pair a1 b1 <= Pair a2 b2 = a1 < a2 || a1 == a2 