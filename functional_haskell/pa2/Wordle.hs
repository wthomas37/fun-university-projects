{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use null" #-}
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
minIndex = 0
maxIndex = 2314

debugLog :: String -> IO ()
debugLog s =
  let debug = False in
  if debug then putStrLn s else return ()

noRepeatLetters :: String -> Bool
noRepeatLetters word =
  undefined

--------------------------------------------------------------------------------
-- Model and Actions

data Model =
  Model () [String] String [String] String Int String Int 
  | Empty deriving (Show)

data Action =
  KeyPress Char






--------------------------------------------------------------------------------
-- Main

-- | arg_checker will return a code depending on what the command arguments
-- are; -1 for random number, n if the input is a valid index n, 
-- -2 if the input index is invalid, and -3 to display the how to play prompt

argChecker :: [String] -> Int
argChecker [] = -1
argChecker [input] 
  | all isDigit input =
    let index = read input :: Int in
      if index >= minIndex && index < maxIndex then index
      else -2
  | input == "--how-to-play" = -4
  | otherwise = -2
argChecker _ = -3




-- | word_fetch will fetch a word depending on an Int input; -1 means
--   a randomly chosen word and any other positive n is an index.
--   the function will continue choosing randomly until a non-repeating char
--   word is found, or in the case of an index being given, will find said
--   word and then return it if it has no repeating characters.

wordFetch :: Int -> IO String 
wordFetch val = do 
  file_content <- readFile "words.txt"
  if val == -1 then do
    rand_val <- randomRIO (minIndex, maxIndex)
    let split = splitOn "\n" file_content in 
      let word = (split !! rand_val) in return word 
  else let split = splitOn "\n" file_content in 
    let word = (split !! val) in return word



howToPlay1 =
  "HOW TO PLAY\n\nGuess the WORDLE in 6 tries.\n\nEach guess must be a valid 5 letter word. Hit the enter button to submit.\n\nExamples\n\n"

howToPlay2 = "  The letter W is in the word and in the correct spot.\n"
howToPlay3 = "  The letter I is in the word but in the wrong spot.\n"
howToPlay4 = "  None of the letters are in the word in any spot."

howToPlay :: IO () 
howToPlay = do
  putStr howToPlay1
  printGuess "wiiii" "weary"
  putStr howToPlay2 
  printGuess "izzzz" "pills"
  putStr howToPlay3 
  printGuess "zzzzz" "vague"
  putStr howToPlay4
  putStrLn ""



errorMessage :: String 
errorMessage = 
  "Usage:\n\ncabal run wordle                     Play random game\ncabal run wordle -- gameNumber       Play specific game\ncabal run wordle -- --how-to-play    Display instructions"

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering 
  args <- getArgs
  let arg_rv = argChecker args in do
    if arg_rv == -2 then die "Invalid game number"
    else if arg_rv == -4 then howToPlay
    else if arg_rv == -3 then die errorMessage
    else do 
      word <- wordFetch arg_rv
      file_content <- readFile "words.txt"
      let wordList = splitOn "\n" file_content in
        let game = Model () wordList (map toLower word) [] "" 0 
                                        "Guess the wordle!" 0 in
          do 
            view game 
            controller game 
            return () 

--------------------------------------------------------------------------------
-- Controller

controller :: Model -> IO Model
controller game = do
  input <- getChar 
  let action = KeyPress input in 
    let new_game = update action game in do
      putStrLn "\x1b[2J"
      view new_game  
      case new_game of 
        Model () _ _ _ _ _ _ status ->
          if status == 0 then controller new_game 
          else return Empty 
  

addChar :: Char -> String -> String 
addChar add str = 
  let str_add = [add] in str ++ str_add 

inWordList :: [String] -> String -> Bool 
inWordList [] guess = False 
inWordList (word : rest) guess
  | map toLower word == map toLower guess = True 
  | otherwise = inWordList rest guess 


winPromptHandler :: [String] -> String 
winPromptHandler board = 
  let len = length board in 
    case len of 
      1 -> "Genius!"
      2 -> "Magnificent!"
      3 -> "Impressive!"
      4 -> "Splendid!"
      5 -> "Great!"
      6 -> "Phew!"
      _ -> error "This should never happen" 


newLineHandler :: Model -> Model 
newLineHandler (Model () wordList answer board curr_guess index _ _)
  | index /= 5 = Model () wordList answer board curr_guess index 
                  "Not enough characters" 0
  | not (inWordList wordList curr_guess) = Model () wordList answer board 
                                          curr_guess index "Not in word list" 0
  | otherwise =
    let new_board = board ++ [map toLower curr_guess] in 
      if map toLower curr_guess == map toLower answer then 
        Model () wordList answer new_board "" 0 (winPromptHandler new_board) 1
      else 
        if length new_board == maxGuesses then 
          Model () wordList answer new_board "" 0 
          ("Bummer, the answer was " ++ map toUpper answer) (-1)
        else Model () wordList answer new_board "" 0 "" 0



update :: Action -> Model -> Model
update (KeyPress input) (Model () wordList answer board curr_guess index _ _)
-- normal letter input 
  | isAlpha input && index < 5 = 
    let new_guess = addChar input curr_guess 
        new_index = index + 1 in 
          Model () wordList answer board new_guess new_index "" 0
-- handling enters
  | input == '\n' = 
    newLineHandler $ Model () wordList answer board curr_guess index "" 0
 -- handling deletes 
  | input == '\DEL' && index > 0 = 
    Model () wordList answer board (take (index - 1) curr_guess) (index - 1) "" 0
-- extraneous cases
  | otherwise = Model () wordList answer board curr_guess index "" 0


--------------------------------------------------------------------------------
-- View

-- handles view stuff

view :: Model -> IO ()
view (Model () wordList answer board curr_guess index prompt _) = do
  putStrLn prompt 
  putStrLn ""
  printBoard answer board curr_guess 
  putStrLn ""
  displayAlpha answer board
  putStrLn ""


-- handles the addition of new characters / priorities to the alphabet tracker

alphaAdder :: (Char, Int) -> [(Char, Int)] -> [(Char, Int)]
alphaAdder (new_char, priority) ((old_char, old_priority) : rest) =
  if new_char == old_char then 
    if priority <= old_priority then (old_char, old_priority) : rest
    else (new_char, priority) : rest 
  else (old_char, old_priority) : alphaAdder (new_char, priority) rest
alphaAdder new [] = [new]


-- for a letter in a guess, return the updated alphabet tracker and 
-- string to obviate the issue of repeated characters 

alphaByLetter :: String -> (Int, Char) -> [(Char, Int)] -> ([(Char, Int)], String)
alphaByLetter answer (i, letter) color_data = 
  let indexed_answer = answer !! i in 
    if letter == indexed_answer then 
      (alphaAdder (letter, 2) color_data, replaceChar answer letter)
    else 
      if elemIndices letter answer /= [] then 
        (alphaAdder (letter, 1) color_data, replaceChar answer letter)
      else (alphaAdder (letter, 0) color_data, replaceChar answer letter)


-- abstract over alphaByLetter to work for whole words

alphaByWord :: String -> String -> [(Char, Int)] -> [(Char, Int)]
alphaByWord answer guess color_data =
  let 
    helper :: String -> Int -> [(Char, Int)] -> [(Char, Int)]
    helper ans count col_dat = 
      if count == wordLength then col_dat
      else let 
        new_col_dat = fst $ alphaByLetter ans (count, guess !! count) col_dat 
        new_ans = snd $ alphaByLetter ans (count, guess !! count) col_dat 
        in helper new_ans (count + 1) new_col_dat 
  in 
    helper answer 0 color_data 


-- abstract over alphaByWord to work for whole boards

alphaByBoard :: String -> [String] -> [(Char, Int)] -> [(Char, Int)]
alphaByBoard answer (first : rest) color_data = 
  alphaByBoard answer rest (alphaByWord answer first color_data)
alphaByBoard _ [] color_data = color_data 


-- lets you know the priority of a character if it is in the 
-- alphabet tracker

inColorData :: Char -> [(Char, Int)] -> Int
inColorData target [] = 3
inColorData target ((letter, priority) : rest) =
  if target == letter then priority 
  else inColorData target rest 

-- helper function to displayAlpha, does the heavy lifting

displayAlphaHelper :: String -> [(Char, Int)] -> IO () 
displayAlphaHelper (first : rest) color_data = 
  let priority = inColorData first color_data in do
    letterHelper priority first 
    displayAlphaHelper rest color_data 
displayAlphaHelper [] color_data = return () 

-- displays the alphabet

displayAlpha :: String -> [String] -> IO () 
displayAlpha answer board = 
  let color_data = alphaByBoard answer board [] 
      alpha = ['a'..'z']
      in displayAlphaHelper alpha color_data 




-- helper function to printBoard
boardHelper :: String -> [String] -> IO () 
boardHelper answer board =
  let 
    recursHelper :: Int -> IO () 
    recursHelper count =
      if count == length board then return ()
      else do
        putStr "\x1b[48;5;16m  \x1b[0m"
        printGuess answer (board !! count)
        putStr "\x1b[48;5;16m  \x1b[0m"
        putStrLn ""
        recursHelper (count + 1)
  in do 
    recursHelper 0 
  


-- fills in the empty rows with the background (black) and then the white empty row
emptyRows :: Int -> IO () 
emptyRows num_rows = 
  if num_rows == 0 then return () 
  else do 
    putStrLn "\x1b[48;5;16m  \x1b[48;5;15m               \x1b[48;5;16m  \x1b[0m"
    emptyRows (num_rows - 1)

-- prints the current guess that is not yet entered
printCurrentGuess :: String -> IO () 
printCurrentGuess curr_guess = 
  let blank_space = wordLength - length curr_guess in 
    let 
      add_space :: String -> Int -> String 
      add_space str num_spaces 
        | num_spaces == 0 = str 
        | otherwise       = add_space (str ++ " ") (num_spaces - 1) 
    in 
      let output = add_space curr_guess blank_space in
        let 
          recursor :: String -> IO () 
          recursor [] = return () 
          recursor (char:chars) = 
            do
              putStr $ "\x1b[48;5;15m\x1b[38;5;16m " ++ [toUpper char] ++ " \x1b[0m"
              recursor chars 
        in do 
          putStr "\x1b[48;5;16m  \x1b[0m" 
          recursor output 
          putStr "\x1b[48;5;16m  \x1b[0m" 



-- prints the whole board 
printBoard :: String -> [String] -> String -> IO ()
printBoard answer board current_guess = do 
  putStrLn "\x1b[48;5;16m                   \x1b[0m"
  boardHelper answer board
  if length current_guess /= 0 then do 
    printCurrentGuess current_guess 
    putStrLn ""
    emptyRows (maxGuesses - length board - 1)
  else do
    emptyRows (maxGuesses - length board )
  putStrLn "\x1b[48;5;16m                   \x1b[0m"


-- prints a guess
printGuess :: String -> String -> IO ()
printGuess answer guess =
  let 
    helper :: String -> Int -> IO () 
    helper ans count =
      if count == wordLength then return ()
      else do
          updated_ans <- printLetter ans (count, guess !! count)
          helper updated_ans (count + 1)
  in do
    helper answer 0 

-- Given a string and a character, it will find the first incidence
-- of said character in the string and replace it with '*' 
replaceChar :: String -> Char -> String 
replaceChar (first : rest) target 
  | first == target = '*' : rest
  | otherwise = first : replaceChar rest target
replaceChar [] target = ""


-- idea here is that the int is a color code; 0 for grey, 1 for yellow,
-- 2 for green, 3 for white 

letterHelper :: Int -> Char -> IO () 
letterHelper 0 letter =
  putStr $ "\x1b[97m\x1b[48;5;8m " ++ [toUpper letter] ++ " \x1b[0m"
letterHelper 1 letter = 
  putStr $ "\x1b[30;43m " ++ [toUpper letter] ++ " \x1b[0m"
letterHelper 2 letter = 
  putStr $ "\x1b[30;42m " ++ [toUpper letter] ++ " \x1b[0m"
letterHelper 3 letter = 
  putStr $ "\x1b[48;5;15m\x1b[38;5;16m " ++ [toUpper letter] ++ " \x1b[0m"
  

-- prints a letter 

printLetter :: String -> (Int, Char) -> IO String 
printLetter answer (i, letter) = 
  let indexed_answer = answer !! i in
    if letter == indexed_answer then do
      letterHelper 2 letter
      return $ replaceChar answer letter 
    else 
      if elemIndices letter answer /= [] then do
        letterHelper 1 letter
        return $ replaceChar answer letter 
      else do
        letterHelper 0 letter 
        return answer









mapIO_ :: (a -> IO b) -> [a] -> IO ()
mapIO_ f as =
  undefined
