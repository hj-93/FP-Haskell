module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

-- | 3.1 Task A0
hand2 :: Hand
hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)

sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size Empty
            , 1 + 1 + 0
            , 2]
{- 
  Output:
    *Main> sizeSteps
   [2,2,2,2,2,2]
-}


-- | 3.4 Task A1
strRank :: Rank -> String
strRank (Numeric n) = show n
strRank r           = show r 

strSuit :: Suit -> String
strSuit suit =
  case suit of
    Spades   -> "\9824"
    Hearts   -> "\9829"
    Diamonds -> "\9830"
    Clubs    -> "\9827"

displayCard :: Card -> String
displayCard card = strRank (rank card) ++ " of " ++ strSuit (suit card)

display :: Hand -> String
display Empty     = ""
display (Add c h) = displayCard c ++ remaining
  where remaining = case h of
          Empty -> "\n"
          _     -> ", " ++ display h


-- | 3.4 Task A2
bustValue :: Integer
bustValue = 21

valueRank :: Rank -> Bool -> Integer
valueRank (Numeric n) _ = n
valueRank Ace ace1Flag
  | ace1Flag            = 1
  | otherwise           = 11
valueRank _ _   = 10

betterValue :: Integer -> Integer -> Integer
betterValue n1 n2
  | n1 > bustValue && n2 > bustValue = min n1 n2
  | n1 > bustValue                   = n2
  | n2 > bustValue                   = n1
  | otherwise                        = max n1 n2

value :: Hand -> Integer
value h = betterValue (sum1 h) (sum2 h) where
  sum1 Empty     = 0
  sum1 (Add c h) = valueRank (rank c) True + sum1 h
  sum2 Empty     = 0
  sum2 (Add c h) = valueRank (rank c) False + sum2 h


-- | 3.4 Task A3
gameOver :: Hand -> Bool
gameOver h = value h > bustValue


-- | 3.4 Task A4
winner :: Hand -> Hand -> Player
winner g b
  | gameOver g                             = Bank
  | not (gameOver b) && value b >= value g = Bank
  | otherwise                              = Guest
