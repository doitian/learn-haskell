module Scrabble
  ( Score (..),
    getScore,
    score,
    scoreString,
    scoreLine,
  )
where

import Data.Char (toLower)
import JoinList

newtype Score n = Score n
  deriving (Eq, Show)

getScore :: Score n -> n
getScore (Score n) = n

instance Functor Score where
  fmap f = Score . f . getScore

instance Applicative Score where
  pure = Score
  Score f <*> Score n = Score $ f n

-- |
-- >>> Score 1 <> Score 2
-- Score 3
instance (Num n) => Semigroup (Score n) where
  (<>) = liftA2 (+)

instance (Num n) => Monoid (Score n) where
  mempty = Score 0

-- | Calculate the score for a single character based on Scrabble tile values.
-- Characters not in the Scrabble alphabet (punctuation, spaces, etc.) score 0.
score :: Char -> Score Int
score c
  | c' `elem` "aeilnorstu" = Score 1
  | c' `elem` "dg" = Score 2
  | c' `elem` "bcmp" = Score 3
  | c' `elem` "fhvwy" = Score 4
  | c' `elem` "k" = Score 5
  | c' `elem` "jx" = Score 8
  | c' `elem` "qz" = Score 10
  | otherwise = Score 0
  where
    c' = toLower c

-- | Calculate the total score for a string.
--
-- >>> scoreString "yay "
-- Score 9
--
-- >>> scoreString "haskell!"
-- Score 14
scoreString :: String -> Score Int
scoreString = mconcat . map score

-- |
-- >>> scoreLine "yay " +++ scoreLine "haskell!"
-- Append (Score 23) (Single (Score 9) "yay ") (Single (Score 14) "haskell!")
scoreLine :: String -> JoinList (Score Int) String
scoreLine s = Single (scoreString s) s
