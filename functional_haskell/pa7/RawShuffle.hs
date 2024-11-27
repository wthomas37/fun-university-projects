module RawShuffle where

import System.Random

removeAt :: Int -> [a] -> (a, [a])
removeAt index list = 
  (list !! index, take index list ++ drop (index + 1) list)

removeRandom :: [a] -> StdGen -> ((a, [a]), StdGen)
removeRandom list g = 
  (removeAt index list, return_gen)
  where 
    new = uniformR (0, len - 1) g
    index = fst new
    return_gen = snd new
    len = length list 


shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle = helper [] where 
  helper :: [a] -> [a] -> StdGen -> ([a], StdGen)
  helper acc [] gen = (acc, gen)
  helper acc init gen = 
    let 
      random_removed = removeRandom init gen
      tuple = fst random_removed
      diminished_list = snd tuple 
      new_gen = snd random_removed
      new_list = acc ++ [fst tuple]
    in 
      helper new_list diminished_list new_gen


