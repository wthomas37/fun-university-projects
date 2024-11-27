module Main where

data Level
  = Undergraduate
  | Masters
  | PhD
  | Other
      deriving Show

name :: String
name = "Will Thomas"

level :: Level
level = Undergraduate

major :: String
major = "Computer Science and Economics"

why :: String
why = "because I enjoyed using racket in 151 and have heard great things about this course."

distance :: Int -> Int -> Int
distance rate time = rate * time

main :: IO ()
main = do
  putStrLn "Hello, world!"
  putStrLn ""
  putStrLn ("My name is " ++ name ++ ".")
  putStrLn ("I am a " ++ show level ++ " student.")
  putStrLn ("I'm studying " ++ major ++ ".")
  putStrLn ("I'm in this class because " ++ why)
  putStrLn ""
  putStrLn $
    "If you travel 15mph for 30 hours you will go " ++
    show (distance 15 30) ++ " miles."
