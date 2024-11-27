{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module HipBarcodes where

-- You may add, remove, or edit imports as you wish. Note the use of
-- qualified imports to avoid collisions between common names. For
-- example, Prelude and Data.Map and Graphics.Image all define `map`.

import           Code128
import           Data.Char
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Graphics.Image (Image, Pixel(..), RGB, VU(VU)) 
import           Graphics.Image.ColorSpace
import           Graphics.Image.Interface (MArray) 
import qualified Graphics.Image as Image
import           System.Environment
import           System.Exit


--------------------------------------------------------------------------------
-- NOTE: Defining these at top-level rather than changing Code128.TheCodes
-- to include them, like the start and stop symbols.

bToC, cToB :: SymbolId
bToC = 99
cToB = 100

data CodeSet = B | C deriving (Show, Eq)


--------------------------------------------------------------------------------
-- 1. General Decoding

-- assume input BC has been checked for start_ stop_ checksum_ issues
decode_helper :: TheCodes -> SymbolId -> [SymbolId] -> [(CodeSet, [SymbolId])]
decode_helper theCodes start_ data_ =
  helper startCode data_ [] [] where 
    startCode = 
      if start_ == startB theCodes then B 
      else C 
    helper :: CodeSet -> [SymbolId] -> [SymbolId] -> 
                      [(CodeSet, [SymbolId])] -> [(CodeSet, [SymbolId])]
    helper curr_code (first : rest) sub_acc acc 
      | first == bToC = helper C rest [] (acc ++ [(curr_code, sub_acc)])
      | first == cToB = helper B rest [] (acc ++ [(curr_code, sub_acc)])
      | otherwise = helper curr_code rest (sub_acc ++ [first]) acc 
    helper curr_code [] sub_acc acc = 
      acc ++ [(curr_code, sub_acc)]

applyDecode :: TheCodes -> (CodeSet, [SymbolId]) -> Either Error String 
applyDecode theCodes (startCode, data_) =
  let 
    start_ = if startCode == B then startB theCodes else startC theCodes 
    stop_ = stop theCodes
    barcode = (start_, data_, computeChecksum start_ data_, stop_)
  in 
    if start_ == startB theCodes then decodeB theCodes barcode 
    else decodeC theCodes barcode 

inputCheck :: Either Error String -> Bool
inputCheck (Left _) = False 
inputCheck _ = True 

decode :: TheCodes -> BC -> Either Error String
decode theCodes (start_, data_, checksum_, stop_)
  | start_ /= startB theCodes &&
    start_ /= startC theCodes = Left $ "decode: bad start: " ++ show start_
  | stop_ /= stop theCodes = Left $ "decode: bad stop: " ++ show stop_
  | checksum_ /= computeChecksum start_ data_ = 
    Left $ "decode: bad checksum: " ++ show checksum_
  | otherwise =
    let 
      decoded = map (applyDecode theCodes) $ decode_helper theCodes start_ data_
      valid = foldr ((&&) . inputCheck) True decoded
    in 
      if valid then Right $ concatMap (\(Right val) -> val) decoded
      else Left "decode: invalid inputs"


--------------------------------------------------------------------------------
-- 2. Optimal Encodings

-- TODO: This is a placeholder definition, to be replaced.
encode :: TheCodes -> String -> Either Error BC
encode theCodes str
  | not $ all isPrintable str = Left "encode: unsupported characters"
  | all isDigit str           = encodeC theCodes str
  | otherwise                 = encodeB theCodes str


--------------------------------------------------------------------------------
-- 3. Making Barcodes

module2Img :: Int -> Int -> Module -> Image VU Y Double 
module2Img imageHeight moduleWidth mod =
    Image.makeImageR VU (imageHeight, moduleWidth) 
    (\(i, j) -> PixelY (val :: Double))
    where 
      val = if mod then 0 else 1

makeBarcode :: FilePath -> Int -> Int -> BCString -> IO ()
makeBarcode filePath imageHeight moduleWidth (BCString symbols)
  | null symbols = error "BCString must not be empty"
  | otherwise =
    let 
      barcode_list = concatMap (map (module2Img imageHeight moduleWidth)) (symbols ++ [[True, True]])
      first = head barcode_list 
      rest = drop 1 barcode_list
      barcode = foldl Image.leftToRight first rest
    in 
      Image.writeImage filePath barcode
  


--------------------------------------------------------------------------------
-- 4. Scanning Barcodes

scanBarcode :: FilePath -> IO BCString
scanBarcode filePath = do 
  img <- Image.readImageY VU filePath
  return $ imageToBCString img 

imageToBCString :: (MArray arr cs e, ToY cs e) => Image arr cs e -> BCString
imageToBCString img = 
  case bcstring of 
    (Just str) -> str 
    _          -> error "this should not be happening"
  where bcstring = maybeReadBCString $ makeBitString img 

moduleWidth :: (MArray arr cs e, ToY cs e) => Image arr cs e -> Int 
moduleWidth img = helper 0 where 
  helper :: Int -> Int 
  helper i
    | isBlack $ Image.index img (0, i) = helper $ i + 1 
    | otherwise = i `div` 2

isBlack :: (ToY cs e) => Pixel cs e -> Bool
isBlack pixel =
  let PixelY y = toPixelY pixel in (y == 0.0)

makeBitString :: (MArray arr cs e, ToY cs e) => Image arr cs e -> String
makeBitString img = helper 0 (Image.cols img) [] where 
  helper :: Int -> Int -> String -> String
  helper curr_index bound acc
    | curr_index >= bound = acc 
    | isBlack $ Image.index img (0, curr_index) = 
      helper (curr_index + mod_width) bound (acc ++ "1")
    | otherwise = 
      helper (curr_index + mod_width) bound (acc ++ "0")
  mod_width = moduleWidth img 




--------------------------------------------------------------------------------
-- 5. Scanning Designed Barcodes

scanDesignedBarcode :: FilePath -> IO BCString
scanDesignedBarcode filePath =
  undefined


--------------------------------------------------------------------------------
-- Main

runEncoder
  :: (TheCodes -> String -> Either Error BC) -> FilePath -> Int -> Int -> String
  -> IO ()
runEncoder f filePath height moduleWidth str = do
  theCodes <- loadTheCodes
  let result = bcToBCString theCodes <$> f theCodes str
  either (const (die "encoder failed")) printEncoding result
    where
      printEncoding bcString = do
        putStrLn $ "\nPayload:\n" ++ str
        putStrLn $ "\nEncoding:\n" ++ show bcString
        makeBarcode filePath height moduleWidth bcString

runDecoder
  :: (TheCodes -> BC -> Either Error String) -> String
  -> IO ()
runDecoder f filePath = do
  theCodes <- loadTheCodes
  bcString <- scanBarcode filePath
  let bc = bcStringToBC theCodes bcString
  either (const (die "decoder failed")) printDecoding (f theCodes bc)
    where
      printDecoding str = do
        putStrLn $ "\nDecoding:\n" ++ str
        putStrLn ""

main :: IO ()
main =
  getArgs >>= processArgs
  where
    processArgs ["encode", filePath, imageHeight, moduleWidth, str] =
      HipBarcodes.runEncoder
        encode filePath (read imageHeight) (read moduleWidth) str
    processArgs ["decode", file] =
      HipBarcodes.runDecoder decode file
    processArgs _ =
      die $ "\nUsage:\n\n"
        ++ "  cabal run hip-barcodes -- encode imageFilePath imageHeight moduleWidth string\n"
        ++ "  cabal run hip-barcodes -- decode imageFilePath\n"
