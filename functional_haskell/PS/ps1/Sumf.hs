
module Sumf where

sumf :: Num n => (a -> n) -> [a] -> n
sumf f [] = 0
sumf f (c:cs) = f c + sumf f cs

square :: Num n => n -> n
square x = x^2
