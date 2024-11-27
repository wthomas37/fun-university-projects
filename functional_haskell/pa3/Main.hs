{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Use intercalate" #-}
{-# HLINT ignore "Use unless" #-}
module Main where

import Data.List
import Interval
import System.IO

---- Input/Output Interface ----

-- Example: splitOn ',' "abc,def,ghi"  => ["abc", "def", "ghi"]

splitOn :: Char -> String -> [String]
splitOn splitChar []      = [[]]
splitOn splitChar (headChar:restChars)
  | splitChar == headChar = [[]] ++ splitOn splitChar restChars
  | otherwise             = (headChar:currentWord):restWords
  where
    currentWord:restWords = splitOn splitChar restChars

-- Vanilla Haskell doesn't allow instances on type synonyms,
-- so we can't make customized Show/Read instances.

readIS :: String -> IntervalSet Int
readIS = map read . splitOn ','

showIS :: IntervalSet Int -> String
showIS = concat . intersperse "," . map show . normalizeIS

-- Combine touching/overlapping regions and remove empty intervals.
-- Inverting twice effectively combines overlapping regions.

normalizeIS :: (Ord a, Bounded a) => IntervalSet a -> IntervalSet a
normalizeIS s
  | simplified == [] = [Range minBound minBound]
  | otherwise        = simplified
  where
    inverse = difference allIS
    simplified = sort . removeEmptyIntervals . inverse . inverse $ s

processLine :: String -> String
processLine line =
  case words line of
    "intersection":rest -> showIS $ intersectionAll $ map readIS rest 
    "union":rest        -> showIS $ unionAll $ map readIS rest 
    "difference":rest   -> showIS $ differenceAll $ map readIS rest 
    "disjoint":rest     -> show $ areAllDisjoint $ map readIS rest
    "equal":rest        -> show $ areAllEqual $ map readIS rest 
    _                   -> "Invalid input"

main :: IO ()
main = do
  input <- getLine
  case input of -- TODO: Examine the line
    'q':_ -> return () -- "q" or "quit" to quit
    _     -> do
      putStrLn $ processLine input 
      hFlush stdout -- "Flush" the output buffer (otherwise Windows may not show output)
      atEnd <- isEOF -- Check if at the end of a file before continuing (for testing)
      if atEnd then
        return ()
      else
        main -- Repeat
