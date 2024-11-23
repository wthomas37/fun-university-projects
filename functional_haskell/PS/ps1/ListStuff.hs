-- | A module to do stuff to lists
module ListStuff where

import Prelude hiding (product)

-- | Given an array of numbers, return the product of all of them

product :: Num n => [n] -> n 
product [] = 1
product (x:xs) = x * product xs 

-- | Given a number, return its square

square :: Num n => n -> n 
square n = n^2

-- to return the product of squares of [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
-- you must run the following:

-- product (map square [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
-- the output is 13168189440000
