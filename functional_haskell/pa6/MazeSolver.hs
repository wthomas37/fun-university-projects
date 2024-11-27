{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module MazeSolver where

import           Data.Map (Map, (!))
import qualified Data.Map as Map
import GHC.Base (VecElem(Int16ElemRep))
import Control.Exception.Base
import Control.Exception

-- Data Representation 
type Path = [Pos]
type Pos  = (Row, Col)
type Row  = Int
type Col  = Int

-- Left refers to special values S, F where True is S and False is F. 
-- Right refers to values Space, X where True is Space and False is X

data Special = Start Visited | Finish Visited deriving (Show, Eq)
data Normal = Space Visited | Wall deriving (Show, Eq)

type Visited = Bool 

type Val = Either Special Normal

type Table = Map Pos Val

data Board = 
  Board 
    { table :: Table
    , rows :: Int 
    , cols :: Int 
    } deriving (Show)

-- IO Helper

add_to_last :: [String] -> Char -> [String]
add_to_last [] add = [[add]]
add_to_last [last] add = [last ++ [add]]
add_to_last (first : rest) add = first : add_to_last rest add

splitOn :: Char -> String -> [String]
splitOn delim = foldl helper [[]]
  where 
    helper acc char
      | char == delim = acc ++ [[]]
      | otherwise = add_to_last acc char 

getVal :: String -> Int -> Val 
getVal row col_index =
  let content = row !! col_index in 
    if content == 'S' then Left $ Start False
    else if content == 'F' then Left $ Finish False
    else if content == ' ' then Right $ Space False
    else if content == 'X' then Right Wall
    else error "invalid characters in maze"


addRowToBoard :: String -> Int -> Table -> Table
addRowToBoard row row_index table = helper 0 where 
  helper :: Int -> Table
  helper col_index
    | col_index >= num_cols = table
    | otherwise = 
      Map.insert pos val (helper $ col_index + 1) where 
        pos = (row_index, col_index)
        val = getVal row col_index 
  num_cols = length row 

addSpace :: String -> Int -> String 
addSpace row col_num 
  | len_row < col_num = addSpace (row ++ " ") col_num 
  | otherwise = row 
  where 
    len_row = length row 

boardHelper :: [String] -> Board 
boardHelper rows = Board (helper 0 Map.empty) num_rows num_cols where 
  helper :: Int -> Table -> Table
  helper count table 
    | count >= num_rows = table
    | otherwise = 
      helper new_count new_table where 
        new_count = count + 1
        new_table = addRowToBoard padded_row count table
        padded_row
          | length row > num_cols = take num_cols row 
          | length row < num_cols = addSpace row num_cols 
          | otherwise = row 
        row = rows !! count 
  num_rows = length rows 
  num_cols = length $ head rows 

-- A note on num_cols; the assumption is valid inputs. num_cols will be how
-- the length of the first row; any rows after that longer than num_cols 
-- will have their excess ignored. If the finish point is in that excess,
-- the board will be considered invalid.

makeBoard :: String -> Maybe Board 
makeBoard maze
  | isValid board = Just board 
  | otherwise = Nothing 
  where board = boardHelper $ splitOn '\n' maze


-- Path Finding 



-- updates the board, marks a certain Pos index as visited
markVisited :: Board -> Pos -> Board
markVisited board index =
  case val of 
    Left  (Finish _) -> Board (Map.update 
                              (\x -> Just (Left (Finish True))) index table') 
                              (rows board) (cols board)

    Left  (Start _)  -> Board (Map.update 
                              (\x -> Just (Left (Start True))) index table') 
                              (rows board) (cols board)

    Right (Space _)  -> Board (Map.update 
                              (\x -> Just (Right (Space True))) index table') 
                              (rows board) (cols board)

    _                -> error "This should not be happening"
  where 
    val = table' ! index 
    table' = table board 

notVisited :: Board -> Pos -> Bool 
notVisited board index = 
  case val of 
    Left  (Start True)  -> False 
    Left  (Finish True) -> False
    Right (Space True)  -> False
    Right  Wall         -> False
    _                   -> True
  where 
    val = table board ! index

isInBounds :: Int -> Int -> Pos -> Bool 
isInBounds num_rows num_cols (r, c) = 
  r >= 0 && r < num_rows && c >= 0 && c < num_cols



getNeighbors :: Board -> Pos -> [Pos]
getNeighbors board (r, c) = 
  let 
    num_rows = rows  board 
    num_cols = cols  board 
    table'   = table board
    rv = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)] 
  in 
    filter (notVisited board) $ filter (isInBounds num_rows num_cols) rv 


-- My exact implementation of DFS took heavy inspiration from:
-- https://www.baeldung.com/cs/dfs-vs-bfs-vs-dijkstra

pathFinder :: Board -> Pos -> Maybe Path 
pathFinder board start
  | table board ! start == Left (Finish False) = Just [start]
  | otherwise = 
    let 
      neighbors = getNeighbors board start 
      helper :: [Pos] -> Maybe [Pos]
      helper [] = Nothing
      helper (first : rest) = 
        let dfs_out = pathFinder (markVisited board start) first in 
          if null dfs_out then helper rest 
          else ([start] ++) <$> dfs_out 
    in 
      helper neighbors  


-- We already know that the board is only made up of spaces, walls, 
-- starts, and finishes. Here, we verify that there are only one 
-- start and one finish; no more no fewer

isValid :: Board -> Bool 
isValid board = helper contents False False where 
  helper :: [(Pos, Val)] -> Bool -> Bool -> Bool 
  helper [] start_found end_found
    | start_found && end_found = True 
    | otherwise = False 
  helper (first : rest) start_found end_found = 
    case first of 
      (_, Left (Start _))  ->
        not start_found && helper rest True end_found
      (_, Left (Finish _)) ->
        not end_found && helper rest start_found True 
      _ -> helper rest start_found end_found 
  contents = Map.assocs $ table board

-- This is called assuming a valid board 
findStart :: Board -> Pos 
findStart board = helper contents where 
  helper :: [(Pos, Val)] -> Pos 
  helper ((pos, val): rest)
    | val == Left (Start False) = pos 
    | otherwise = helper rest 
  contents = Map.assocs $ table board 

-- Driver Code

{- https://stackoverflow.com/questions/57219021/
    how-is-join-implemented-in-maybe-list-monad
-}

join :: Monad m => m (m a) -> m a
join m = m >>= id



checkPath :: Maybe Path -> Board -> Bool 
checkPath Nothing _ = False 
checkPath (Just (first : rest)) board = check first && helper first rest where 
  helper :: Pos -> Path -> Bool 
  helper first (second : rest)
    | not $ isInBounds (rows board) (cols board) second = False 
    | second `notElem` getNeighbors board first = False 
    | otherwise = True 
  check :: Pos -> Bool 
  check f = f == findStart board

main :: IO ()
main = do
  putStrLn "Insert Maze FileName: (mazes/ and .txt will be added automatically)"
  fileName <- getLine 
  contents <- try @IOException $ readFile ("mazes/" ++ fileName ++ ".txt")
  case contents of 
    Left except -> do 
      putStrLn "Invalid filePath, try again"
      main
    Right maze -> do
      let 
        board = makeBoard maze
        path = join $ (pathFinder <$> board) <*> (findStart <$> board)
      print path 
      putStrLn "Testing path"
      let output = checkPath path <$> board 
      if output == Just True 
        then putStrLn "Path is Valid!" else putStrLn "Path is Invalid."
      pure ()
