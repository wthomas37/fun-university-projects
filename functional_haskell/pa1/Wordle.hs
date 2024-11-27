{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use list literal pattern" #-}
{-# HLINT ignore "Redundant if" #-}
module Main where

import System.IO
import System.Environment
import Data.Char
import System.Exit
import System.Random
import Data.List.Split
import Data.List

maxGuesses = 6
wordLength = 5
min_index = 0
max_index = 2314

debugLog :: String -> IO ()
debugLog s =
  let debug = False in
  if debug then putStrLn s else return ()

noRepeatLetters :: String -> Bool
noRepeatLetters word =
  let nubbed = nub word in word == nubbed

--------------------------------------------------------------------------------
-- Main

-- | arg_checker will return a code depending on what the command arguments
-- are; -1 for random number, n if the input is a valid index n, 
-- -2 if the input index is invalid, and -3 to display the how to play prompt

arg_checker :: [String] -> Int
arg_checker [] = -1
arg_checker [input] 
  | (all isDigit input) =
    let index = read input :: Int in
      if index >= min_index && index < max_index then index
      else -2
  | input == "--how-to-play" = -4
  | otherwise = -2
arg_checker _ = -3




-- | word_fetch will fetch a word depending on an Int input; -1 means
--   a randomly chosen word and any other positive n is an index.
--   the function will continue choosing randomly until a non-repeating char
--   word is found, or in the case of an index being given, will find said
--   word and then return it if it has no repeating characters.

word_fetch :: Int -> IO String 
word_fetch val = do 
  file_content <- readFile "words.txt"
  if val == -1 then do
    rand_val <- randomRIO (min_index, max_index)
    let split = splitOn "\n" file_content in 
      let word = (split !! rand_val) in 
        if (noRepeatLetters word) then return word 
        else word_fetch (-1)
  else let split = splitOn "\n" file_content in 
    let word = (split !! val) in 
      if (noRepeatLetters word) then return word
      else die ((map toUpper word) ++ " has repeat letters")


how_to_play :: String 
how_to_play =
  "HOW TO PLAY\n\nGuess the WORDLE in 6 tries.\n\nEach guess must be a valid 5 letter word. Hit the enter button to submit.\n\nExamples\n\n W [e][a][r][y]  The letter W is in the word and in the correct spot.\n[p] i [l][l][s]  The letter I is in the word but in the wrong spot.\n[v][a][g][u][e]  None of the letters are in the word in any spot."


error_message :: String 
error_message = 
  "Usage:\n\n./wordle                  Play random game\n./wordle gameNumber       Play specific game\n./wordle --how-to-play    Display instructions"


main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  args <- getArgs
  let arg_rv = arg_checker args in
    if arg_rv == -2 then die "Invalid game number"
    else if arg_rv == -4 then putStrLn how_to_play
    else if arg_rv == -3 then die error_message 
    else do 
      word <- word_fetch arg_rv
      file_content <- readFile "words.txt"
      let word_list = splitOn "\n" file_content in
        do 
          printBoard word []
          printPrompt "Guess the wordle!"
          controller word_list word [] 


  


--------------------------------------------------------------------------------
-- Controller

controller :: [String] -> String -> [String] -> IO ()
controller wordList answer board = do
  guess <- getLine
  let update_response = maybeUpdateBoard wordList answer board guess in 
    let prompt = fst update_response
        new_board = snd update_response in 
          do 
            printPrompt prompt
            putStrLn ""
            printBoard answer new_board 
            --check for ending, decide on caption--
            if length new_board == length board 
              then do 
                printPrompt "Next guess?"
                controller wordList answer new_board 
            else 
              let num_guesses = length new_board 
                  newest_word = last new_board in
                    if newest_word == answer then win_prompt_handler num_guesses 
                    else if num_guesses == maxGuesses then 
                      printPrompt $ "Bummer, the answer was "++(map toUpper answer)
                    else do 
                      printPrompt "Next guess?"
                      controller wordList answer new_board 
                   
win_prompt_handler :: Int -> IO () 
win_prompt_handler num_guesses 
  | num_guesses == 1 = printPrompt "Genius!"
  | num_guesses == 2 = printPrompt "Magnificent!"
  | num_guesses == 3 = printPrompt "Impressive!"
  | num_guesses == 4 = printPrompt "Splendid!"
  | num_guesses == 5 = printPrompt "Great!"
  | num_guesses == 6 = printPrompt "Phew!"


allLetters :: String -> Bool 
allLetters (c : [])
  | isLetter c = True 
  | otherwise = False 
allLetters (c : cs)
  | isLetter c = allLetters cs 
  | otherwise = False 
allLetters _ = False 


all_lowercase :: String -> Bool 
all_lowercase word = 
  let lower_word = map toLower word in 
    if lower_word == word then True else False 

in_word_list :: [String] -> String -> Bool 
in_word_list [] guess = False 
in_word_list (word : rest) guess
  | map toLower word == map toLower guess = True 
  | otherwise = in_word_list rest guess 



maybeUpdateBoard
  :: [String] -> String -> [String] -> String -> (String, [String])
maybeUpdateBoard wordList answer board guess 
  | not (allLetters guess) = ("Not all lowercase letters", board)
  | length guess > 5 = ("Too many letters", board)
  | length guess < 5 = ("Not enough letters", board)
  | not (all_lowercase guess) = ("Not all lowercase letters", board)
  | not (noRepeatLetters guess) = ("Repeat letters not allowed", board)
  | not (in_word_list wordList guess) = ("Not in word list", board)
  | otherwise = ("", board ++ [guess])


--------------------------------------------------------------------------------
-- View


printPrompt :: String -> IO ()
printPrompt str = 
  if str /= "" then do 
    putStrLn str else return ()

charToString :: Char -> String 
charToString c = [c]

board_helper :: String -> [String] -> IO () 
board_helper answer board =
  let 
    recurs_helper :: Int -> IO () 
    recurs_helper count =
      if count == length board then return ()
      else do
        putStr "##"
        printGuess answer (board !! count)
        putStr "##"
        putStrLn ""
        recurs_helper (count + 1)
  in do 
    recurs_helper 0 
  
empty_rows :: Int -> IO () 
empty_rows num_rows = 
  if num_rows == 0 then return () 
  else do 
    putStrLn "##               ##"
    empty_rows (num_rows - 1)


printBoard :: String -> [String] -> IO ()
printBoard answer board = do 
  putStrLn "###################"
  board_helper answer board
  let extra_rows = maxGuesses - (length board) in
    empty_rows extra_rows 
  putStrLn "###################\n"

printGuess :: String -> String -> IO ()
printGuess answer guess =
  let 
    helper :: Int -> IO () 
    helper count =
      if count == wordLength then return ()
      else do
        printLetter answer (count, guess !! count)
        helper (count + 1)
  in do
    helper 0 



printLetter :: String -> (Int, Char) -> IO ()
printLetter answer (i, letter) = 
  let indexed_answer = answer !! i in
    if letter == indexed_answer then 
      putStr $ " " ++ charToString (toUpper letter) ++ " "
    else 
      if elemIndices letter answer /= [] then 
      putStr $ " " ++ (charToString letter) ++ " "
      else putStr ("[" ++ (charToString letter) ++ "]")


mapIO_ :: (a -> IO b) -> [a] -> IO ()
mapIO_ f as =
  undefined
