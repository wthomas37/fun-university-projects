module MonadicShuffle where

import StateFunc
import System.Random

type RandFunc a = StateFunc StdGen a

removeAt :: Int -> [a] -> (a, [a])
removeAt index list = 
  (list !! index, take index list ++ drop (index + 1) list)
  
removeRandom :: [a] -> RandFunc (a, [a])
removeRandom list = do
  g0 <- get
  let (index, return_gen) = uniformR (0, length list - 1) g0
  put return_gen
  pure $ removeAt index list 

shuffle :: [a] -> RandFunc [a]
shuffle = helper [] where 
  helper acc init = do 
    (removed, diminished) <- removeRandom init
    let new_acc = acc ++ [removed]
    if null diminished then pure new_acc 
    else helper new_acc diminished



