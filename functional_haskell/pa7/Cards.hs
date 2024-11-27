module Cards where

data CardValue
  = King
  | Queen
  | Jack
  | PipCard Int -- From 1 to 10
      deriving (Show, Eq)

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
      deriving (Show, Eq, Enum)

data AcesHigh = AcesHigh
data AcesLow  = AcesLow

data Card (a :: *) =
  Card CardValue Suit
    deriving (Eq)

instance Show (Card a) where
  show (Card value suit) = showValue value ++ showSuit suit
    where
      showValue (PipCard 1)  = "A"
      showValue (PipCard n)  = show n
      showValue Jack         = "J" 
      showValue Queen        = "Q" 
      showValue King         = "K" 

      showSuit Clubs         = "♣"
      showSuit Diamonds      = "♦"
      showSuit Hearts        = "♥" -- "\x1B[31m♥\x1B[0m" -- red in terminal
      showSuit Spades        = "♠" -- "\x1B[31m♠\x1B[0m"

   {- Uncomment if you don't have Unicode:

      showSuit Clubs         = "C"
      showSuit Diamonds      = "D"
      showSuit Hearts        = "H"
      showSuit Spades        = "S"
   -}


--------------------------------------------------------------------------------
-- TODO: Uncomment and implement the following two instances


instance Ord Suit where 
  (<=) :: Suit -> Suit -> Bool 
  suit1 <= suit2 
    | suit1 == suit2 = True 
  _ <= Spades = True 
  suit <= Hearts
    | suit == Spades = False 
    | otherwise = True 
  suit <= Diamonds
    | suit == Hearts || suit == Spades = False 
    | otherwise = True 
  _ <= _ = False 

instance Ord (Card AcesHigh) where
  (<=) :: Card AcesHigh -> Card AcesHigh -> Bool
  card1@(Card val1 suit1) <= card2@(Card val2 suit2)
    | card1 == card2 = True 
    | suit1 == suit2 = 
      case (val1, val2) of 
        (PipCard int1, PipCard int2) 
          | int1 /= 1 && int2 /= 1 -> int1 <= int2 
          | int1 == 1 -> False 
          | otherwise -> True
        (PipCard int, _)
          | int == 1 -> False 
          | otherwise -> True
        (_, PipCard int)
          | int == 1 -> True 
          | otherwise -> False 
        (_, King) -> True 
        (face, Queen)
          | face == King -> False 
          | otherwise -> True 
        _ -> False 
    | otherwise = suit1 <= suit2


instance Ord (Card AcesLow) where
  (<=) :: Card AcesLow -> Card AcesLow -> Bool
  card1@(Card val1 suit1) <= card2@(Card val2 suit2)
    | card1 == card2 = True 
    | suit1 == suit2 = 
      case (val1, val2) of 
        (PipCard int1, PipCard int2) -> int1 <= int2 
        (PipCard int, _) -> True
        (_, PipCard int) -> False 
        (_, King) -> True 
        (face, Queen)
          | face == King -> False 
          | otherwise -> True 
        _ -> False 
    | otherwise = suit1 <= suit2 


--------------------------------------------------------------------------------

fullDeckAcesHigh :: [Card AcesHigh]
fullDeckAcesLow  :: [Card AcesLow]

fullDeckAcesHigh = fullDeck True
fullDeckAcesLow  = fullDeck False

faces :: [CardValue]
faces = [Jack, Queen, King]

enumHelp :: Bool -> [CardValue]
enumHelp False = [PipCard i | i <- [1 .. 10]] ++ faces 
enumHelp _ = [PipCard i | i <- [2 .. 10]] ++ faces ++ [PipCard 1]

-- All 52 cards in ascending order, treating aces high (True) or low (False).
fullDeck :: Bool -> [Card a]
fullDeck True = [Card i j | i <- enumHelp True,  j <- [Clubs ..]]
fullDeck _    = [Card i j | i <- enumHelp False, j <- [Clubs ..]]
