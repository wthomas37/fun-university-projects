module RandomWars where

import Cards
import MonadicShuffle
import StateFunc
import System.Random

howLongDoWarsLast :: Int
howLongDoWarsLast =
  undefined

main :: IO ()
main = do
  stdGen <- initStdGen
  let ns = evalStateFunc (simulateGames undefined) stdGen
  undefined


simulateGames :: Int -> RandFunc [Int]
simulateGames num_games = undefined

