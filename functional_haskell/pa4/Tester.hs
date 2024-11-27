{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use unwords" #-}
module Tester where

import Control.Monad
import Code128
import Data.List

data Test a b = -- data, instead of type, for better error messages
  Test
    { funcName :: String
    , func     :: a -> Either Error b
    , input    :: a
    , output   :: Either () b -- () because we ignore the actual Error strings
    }

main :: IO ()
main = do
  theCodes <- loadTheCodes
  runTests $ tests_encodeB theCodes
  runTests $ tests_encodeC theCodes
  runTests $ tests_decodeB theCodes
  runTests $ tests_decodeC theCodes
  runTests $ tests_encodeAndShow theCodes
  runTests $ tests_readAndDecode theCodes

runTests :: (Show a, Eq b) => [Test a b] -> IO ()
runTests tests = do
  putStrLn ""
  forM_ tests $ \(Test funcName func input output) ->
    let                -------------------------- shadowing the record selectors
      strInput =
        show input
      strInput'
        | length strInput < 40 = strInput
        | otherwise            = take 40 strInput ++ "..."
      success b =
        putStrLn $ displayColor (if b then Green else Red) $
          intercalate " " [if b then "PASS" else "FAIL", funcName, strInput']
    in
    case (func input, output) of
      (Left _, Left ())   -> success True
      (Right b, Right b') -> if b == b' then success True else success False
      _                   -> success False

--------------------------------------------------------------------------------

data Color = Red | Green deriving (Eq, Ord)

displayColor :: Color -> String -> String
displayColor color str =
  let
    -- https://en.wikipedia.org/wiki/ANSI_escape_code
    -- https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
    green  = "\ESC[32m"
    red    = "\ESC[31m"
    reset  = "\ESC[0m"
  in
    case color of
      Red   -> red ++ str ++ reset
      Green -> green ++ str ++ reset

--------------------------------------------------------------------------------

piString = "3141592653"

tests_encodeB :: TheCodes -> [Test String BC]
tests_encodeB theCodes =
  map (uncurry $ Test "encodeB theCodes" $ encodeB theCodes)
    [ ( "Haskell!"
      , Right (104,[40,65,83,75,69,76,76,1],1,106)
      )
    , ( "CMSC 22300? HOT Programming in Haskell!"
      , Right (104,[35,45,51,35,0,18,18,19,16,16,31,0,40,47,52,0,48,82,79,71,82,65,77,77,73,78,71,0,73,78,0,40,65,83,75,69,76,76,1],72,106)
      )
    , ( "Separation of ⛪ and 🏛"
      , Left ()
      )
    , ( piString
      , Right (104,[19,17,20,17,21,25,18,22,21,19],88,106)
      )
    , ( "Wikipedia"
      , Right (104,[55,73,75,73,80,69,68,73,65],88,106)
      )
    , ( "Wikiped\n"
      , Left ()
      )
    ]

tests_encodeC :: TheCodes -> [Test String BC]
tests_encodeC theCodes =
  map (uncurry $ Test "encodeC theCodes" $ encodeC theCodes)
    [ ( "Haskell!"
      , Left ()
      )
    , ( piString
      , Right (105,[31,41,59,26,53],43,106)
      )
    , ( "22300"
      , Left ()
      )
    , ( "\n"
      , Left ()
      )
    , ( "12345678"
      , Right (105,[12,34,56,78],47,106))
    ]

tests_decodeB :: TheCodes -> [Test BC String]
tests_decodeB theCodes =
  map (uncurry $ Test "decodeB theCodes" $ decodeB theCodes)
    [ ( (104,[40,65,83,75,69,76,76,1],1,106)
      , Right "Haskell!"
      )
    , ( (104,[19,17,20,17,21,25,18,22,21,19],88,106)
      , Right "3141592653"
      )
    , ( (104,[19,17,20,17,21,25,18,22,21,19],20,106) -- bad checksum
      , Left ()
      )
    , ( (104,[55,73,75,73,80,69,68,73,65],88,106)
      , Right "Wikipedia"
      )
    , ( (105,[55,73,75,73,80,69,68,73,65],88,106) -- bad start
      , Left ()
      )
    ]

tests_decodeC :: TheCodes -> [Test BC String]
tests_decodeC theCodes =
  map (uncurry $ Test "decodeC theCodes" $ decodeC theCodes)
    [ ( (105,[31,41,59,26,53],43,106)
      , Right "3141592653"
      )
    , ( (105,[31,41,59,26,53],42,106) -- bad checksum
      , Left ()
      )
    , ( (0, [], 0, 0)
      , Left ()
      )
    , ( (105,[31,41,59,26,53],43,105) -- bad end code
      , Left ()
      )
    , ( (105,[12,34,56,78],47,106)
      , Right "12345678"
      )
    ]

tests_encodeAndShow :: TheCodes -> [Test String String]
tests_encodeAndShow theCodes =
  map (uncurry $ Test "encodeAndShow encodeB" $ encodeAndShow encodeB theCodes)
    [ ( "Wikipedia"
      , Right "11010010000111010001101000011010011000010010100001101001010011110010110010000100001001101000011010010010110000111100100101100011101011"
      )
    , ( "Hello!"
      , Right "11010010000110001010001011001000011001010000110010100001000111101011001101100100100111101100011101011"
      )
    , ( "\"Hello!\""
        -- NOTE: To `cabal run` this test, need to escape as follows:
        -- % cabal run code128 -- encodeB \""Hello\!"\"
      , Right "110100100001100110011011000101000101100100001100101000011001010000100011110101100110110011001100110111001100101100011101011"
      )
    , ( "\n"
      , Left ()
      )
    , ( "the quick brown fox123"
      , Right "1101001000010011110100100110000101011001000011011001100100101111001001111001010000110100100001011001100001001011011001100100100001101001001111010001111010111100101001100001010011011001100101100001001000111101011110010010100111001101100111001011001011100110011101001100011101011"
      )
    ]

tests_readAndDecode :: TheCodes -> [Test String String]
tests_readAndDecode theCodes =
  map (uncurry $ Test "readAndDecode decodeB" $ readAndDecode decodeB theCodes)
    [ ( "11010010000111010001101000011010011000010010100001101001010011110010110010000100001001101000011010010010110000111100100101100011101011"
      , Right "Wikipedia"
      )
    , ( "110100100001110100011010000110100110000100101000011010010100111100101100100001000010011010000110100100101100001111001001011000111010" -- missing final bar
      , Left ()
      )
    , ( ""
      , Left ()
      )
    , ( "1101001000010011110100100110000101011001000011011001100100101111001001111001010000110100100001011001100001001011011001100100100001101001001111010001111010111100101001100001010011011001100101100001001000111101011110010010100111001101100111001011001011100110011101001100011101011"
      , Right "the quick brown fox123"
      )
    , ( "110100100001100110011011000101000101100100001100101000011001010000100011110101100110110011001100110111001100101100011101011"
      , Right "\"Hello!\""
      )
    ]
