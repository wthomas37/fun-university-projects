-- | A sample module from the book to be modified

module Main where


import Data.Char 
import System.Environment

capitalize :: String -> String
capitalize [] = []
capitalize (first:rest) = ((Data.Char.toUpper first) :rest)

main :: IO ()
main = do
    putStrLn "Hello. I am a HAL 9000 series computer."
    name <- getEnv "USER"
    putStrLn $ "Good morning, " ++ (capitalize name) ++ "."