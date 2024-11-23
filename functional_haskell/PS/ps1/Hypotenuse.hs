-- | A module for working with triangles.

module Hypotenuse where

-- | Compute the length of the hypotenuse of a triangle from the lengths
--   of its sides.

hypotenuse :: Double -> Double -> Double
hypotenuse a b = sqrt (square a + square b)

-- | Square a number.

square :: Num n => n -> n
square x = x ^ 2

-- | Compute edge length c using the law of cosines
--   inputs are sides a, b, and angle gamma in degrees

law_of_cosines :: Double -> Double -> Double -> Double
law_of_cosines a b gamma = sqrt ((square a + square b) - 
                        (2 * a * b * cos(gamma * pi / 180)))
