{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use last" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use section" #-}
module Code128 where

import           Data.Char
import           Data.List
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           System.Environment
import           System.Exit


--------------------------------------------------------------------------------
-- Data Representation

data BCString =            -- "Barcode String"
  BCString [Symbol]        --   Start, data, checksum, and stop symbols.
                           --   The final bar ("11") is implicit.
                           --   No quiet zones.

type Symbol   = [Module]   -- Always length 11
type Module   = Bool       -- True/False means part of bar/space

type BC =                  -- "Barcode" (internal representation)
  ( SymbolId               --   Start symbol
  , [SymbolId]             --   Encoded data
  , SymbolId               --   Checksum
  , SymbolId               --   Stop symbol
  )                        --   Final bar is implicit

type SymbolId = Int
type BValue   = Either Char String
type CValue   = Either (Char, Char) String

data TheCodes =
  TheCodes
    { startB       :: SymbolId
    , startC       :: SymbolId
    , stop         :: SymbolId
    , idsToSymbols :: Map SymbolId Symbol
    , symbolsToIds :: Map Symbol SymbolId
    , bEncodings   :: Map Char SymbolId
    , cEncodings   :: Map (Char, Char) SymbolId
    , bDecodings   :: Map SymbolId BValue
    , cDecodings   :: Map SymbolId CValue
    } deriving (Show)

type Error = String


--------------------------------------------------------------------------------
-- 1. Data Loading

loadTheCodes :: IO TheCodes
loadTheCodes = do
  rows <- map (splitOn ',') <$> lines <$> readFile "code128.csv"
  return $ rowsToCodes $ dropFirstAndLast rows

rowsToCodes :: [[String]] -> TheCodes
rowsToCodes rows = 
  -- Perfect opportunity for NamedFieldPuns. Try it!
  TheCodes
    { startB = 104, startC = 105, stop = 106
    , idsToSymbols = idsToSymbols
    , symbolsToIds = symbolsToIds
    , bEncodings = bEncodings
    , cEncodings = cEncodings
    , bDecodings = bDecodings
    , cDecodings = cDecodings
    }

  where
    (idsToSymbols, symbolsToIds, bEncodings, cEncodings, bDecodings, cDecodings) =
      foldr processRow
        (Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)
        rows

    processRow row accumulators =
      accumulators' where

        [_id, _pattern, _, _, _bValue, _cValue] =
          row

        (idsToSymbols, symbolsToIds, bEncodings, cEncodings, bDecodings, cDecodings) =
          accumulators

        accumulators' =
          (idsToSymbols', symbolsToIds', bEncodings', cEncodings', bDecodings', cDecodings')

        symbolId = read _id :: Int

        idsToSymbols' = Map.insert symbolId (readSymbol _pattern) idsToSymbols 

        symbolsToIds' = Map.insert (readSymbol _pattern) symbolId symbolsToIds

        bEncodings' =  
          let value = readBValue _bValue in 
            case value of 
              Left char -> Map.insert char symbolId bEncodings 
              _ -> bEncodings

        cEncodings' = 
          let value = readCValue _cValue in 
            case value of 
              Left chars -> Map.insert chars symbolId cEncodings 
              _ -> cEncodings

        bDecodings' = 
          let value = readBValue _bValue in 
            case value of 
              Left char -> Map.insert symbolId value bDecodings 
              _ -> bDecodings 

        cDecodings' = 
          let value = readCValue _cValue in 
            case value of 
              Left char -> Map.insert symbolId value cDecodings 
              _ -> cDecodings 



add_to_last :: [String] -> Char -> [String]
add_to_last [] add = [[add]]
add_to_last [last] add = [last ++ [add]]
add_to_last (first : rest) add = (first : add_to_last rest add)

splitOn :: Char -> String -> [String]
splitOn delim = foldl helper [[]]
  where 
    helper acc char
      | char == delim = acc ++ [[]]
      | otherwise = add_to_last acc char 


dropFirstAndLast :: [a] -> [a]
dropFirstAndLast xs = take (len - 2) (drop 1 xs) 
  where len = length xs

readSymbol :: String -> Symbol
readSymbol = map helper where 
  helper char 
    | char == '1' = True 
    | char == '0' = False

readBValue :: String -> BValue
readBValue str = 
  case length str of 
    1 -> Left $ head str 
    _ -> 
      if str == "space" then Left ' '
      else if str == "comma" then Left ','
      else Right str

readCValue :: String -> CValue
readCValue str =
  case length str of 
    2 -> Left (head str, last str)
    _ -> Right str 


--------------------------------------------------------------------------------
-- 2. Basic Encodings

encodeB :: TheCodes -> String -> Either Error BC
encodeB theCodes str =
  if not $ all isPrintable str then (Left "unsupported characters")
  else 
    let inner = (map ((bEncodings theCodes) !) str)
    in 
      Right (start_val, inner, computeChecksum start_val inner, stop_val)
      where 
        start_val = (startB theCodes)
        stop_val = (stop theCodes)



encodeC :: TheCodes -> String -> Either Error BC
encodeC theCodes str 
  | odd $ length str = Left "encodeC: odd number of characters"
  | otherwise = 
    let inner = sequenceMaybe $ map (flip Map.lookup (cEncodings theCodes)) (adjacentPairs str)
    in case inner of 
      Nothing         -> Left "encodeC: unsupported characters"
      Just inner_data ->
        Right (start_val, inner_data, computeChecksum start_val inner_data, stop_val)
        where 
          start_val = (startC theCodes)
          stop_val = (stop theCodes)

computeChecksum :: SymbolId -> [SymbolId] -> Int
computeChecksum symbol symbols =
  mod (symbol + sum (zipWith (*) [1..] symbols)) 103
  


isPrintable :: Char -> Bool
isPrintable c = 
  val >= 32 && val <= 126
  where val = ord c

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs (first : (second : rest)) = (first, second) : adjacentPairs rest 
adjacentPairs [] = []

sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe maybes = 
  let helper :: [Maybe a] -> [a] 
      helper (Just helmet : rest) = helmet : helper rest 
      helper _ = []
  in let helped = helper maybes in 
    if length maybes == length helped then Just helped 
    else Nothing 


--------------------------------------------------------------------------------
-- 3. Basic Decodings



decodeB :: TheCodes -> BC -> Either Error String
decodeB theCodes (start_, data_, checksum_, stop_)
  | start_ /= (startB theCodes) = Left $ "decodeB: bad start: " ++ (show start_)
  | stop_ /= (stop theCodes) = Left $ "decodeB: bad stop: " ++ (show stop_)
  | computeChecksum start_ data_ /= checksum_ =
    Left $ "decodeB: bad checksum: " ++ (show checksum_)
  | otherwise =
    let temp = sequenceMaybe $ map (flip Map.lookup (bDecodings theCodes)) data_ in
      case temp of 
        Nothing -> Left "decodeB: invalid characters"
        Just stuff -> 
          Right $ map (\(Left val) -> val) stuff 



decodeC :: TheCodes -> BC -> Either Error String
decodeC theCodes (start_, data_, checksum_, stop_)
  | start_ /= (startC theCodes) = Left $ "decodeC: bad start: " ++ (show start_)
  | stop_ /= (stop theCodes) = Left $ "decodeC: bad stop: " ++ (show stop_)
  | computeChecksum start_ data_ /= checksum_ =
    Left $ "decodeB: bad checksum: " ++ (show checksum_)
  | otherwise = 
    let temp = sequenceMaybe $ map (flip Map.lookup (cDecodings theCodes)) data_ in 
      case temp of 
        Nothing -> Left "decodeC: invalid characters"
        Just stuff -> 
          Right $ concatMap (\(Left (a,b)) -> [a, b]) stuff 



--------------------------------------------------------------------------------
-- 4. Barcode String Manipulation

finalBar     = "11"
symbolLength =  11

symbolToString :: Symbol -> String 
symbolToString = map helper where 
  helper :: Bool -> Char 
  helper bool 
    | bool = '1'
    | not bool = '0'

instance Show BCString where
  show :: BCString -> String
  show (BCString symbols) = (concatMap symbolToString symbols) ++ "11"

instance Read BCString where
  readsPrec :: Int -> String -> [(BCString, String)]
  readsPrec _ str =
    case maybeReadBCString str of
      Just bcString -> [(bcString, "")]
      Nothing       -> []

isZeroOrOne :: Char -> Bool 
isZeroOrOne c 
  | c == '0' || c == '1' = True 
  | otherwise = False


-- inspiration: https://stackoverflow.com/questions/12876384/grouping-a-list-into-lists-of-n-elements-in-haskell
everyEleven :: Int -> [a] -> [[a]]
everyEleven _ [] = []
everyEleven num list 
  | num > 0 = (take num list) : (everyEleven num $ drop num list)
  | otherwise = error "not a multiple of num"




maybeReadBCString :: String -> Maybe BCString
maybeReadBCString str
  | length str < 3 = Nothing
  | not $ all isZeroOrOne str = Nothing 
  | (length (take (length str - 2) str) `mod` 11) /= 0 = Nothing 
  | otherwise =
    let 
      helper :: Char -> Bool 
      helper c 
        | c == '1' = True 
        | otherwise = False
    in let 
      temp = take (length str - 2) $ map helper str 
      in (Just $ BCString $ everyEleven 11 temp)






--------------------------------------------------------------------------------

run :: (TheCodes -> a -> Either Error b) -> a -> IO (Either Error b)
run f a = do
  theCodes <- loadTheCodes
  pure $ f theCodes a


--------------------------------------------------------------------------------
-- User Interface

bcToBCString :: TheCodes -> BC -> BCString
bcToBCString theCodes (start, data_, checksum, stop) =
  let symbolIds = [start] ++ data_ ++ [checksum, stop] in
  BCString $ map (\i -> (idsToSymbols theCodes) ! i) symbolIds

bcStringToBC :: TheCodes -> BCString -> BC
bcStringToBC theCodes (BCString symbols) =
  (start, data_, checksum, stop)
    where
      list     = map (\symbol -> (symbolsToIds theCodes) ! symbol) symbols
      start    = head list
      stop     = head $ reverse list
      checksum = head $ tail $ reverse list
      data_    = reverse $ tail $ tail $ reverse $ tail list

encodeAndShow
  :: (TheCodes -> String -> Either Error BC) -> TheCodes -> String
  -> Either Error String
encodeAndShow f theCodes str =
  show . bcToBCString theCodes <$> f theCodes str

readAndDecode
  :: (TheCodes -> BC -> Either Error String) -> TheCodes -> String
  -> Either Error String
readAndDecode f theCodes str =
  -- Call `maybeReadBCString` instead of `read` because the latter may crash
  case maybeReadBCString str of
    Nothing       -> Left "no parse"
    Just bcString ->
      let barcode = bcStringToBC theCodes $ bcString in
      f theCodes barcode

runEncoder
  :: (TheCodes -> String -> Either Error BC) -> String
  -> IO ()
runEncoder f str = do
  theCodes <- loadTheCodes
  case encodeAndShow f theCodes str of
    Left error -> die error
    Right s    -> putStrLn s

runDecoder
  :: (TheCodes -> BC -> Either Error String) -> String
  -> IO ()
runDecoder f str = do
  theCodes <- loadTheCodes
  case readAndDecode f theCodes str of
    Left error -> die error
    Right str  -> putStrLn str

main = do
  theCodes <- loadTheCodes
  args <- getArgs
  case args of
    ["encodeB", str] -> runEncoder encodeB str
    ["decodeB", str] -> runDecoder decodeB str
    ["encodeC", str] -> runEncoder encodeC str
    ["decodeC", str] -> runDecoder decodeC str
    _                -> die "Usage: cabal run code128 -- {en,de}code{B,C} string"
                         -- "Usage: ./Code128 {en,de}code{B,C} string"
