{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ScrabbleBuffer (ScrabbleBuffer) where

import Buffer
import Data.Foldable (fold)
import JoinList
import Scrabble
import Sized

type Lines = Size

type ScrabbleBuffer = JoinList (Score Int, Lines) String

-- |
-- >>> let buf :: ScrabbleBuffer = fromString "yay\nhaskell!"
-- >>> line 0 buf
-- Just "yay"
-- >>> line 1 buf
-- Just "haskell!"
-- >>> line 2 buf
-- Nothing
-- >>> toString $ replaceLine 0 "rocks" buf
-- "rocks\nhaskell!\n"
-- >>> numLines buf
-- 2
-- >>> value buf
-- 23
instance Buffer ScrabbleBuffer where
  toString = unlines . foldMap (: [])

  fromString = mconcat . fmap toLine . lines
    where
      toLine s = Single (scoreString s, Size 1) s

  line n b
    | 0 <= n && n < numLines b = Just $ fold . takeJ 1 . dropJ n $ b
    | otherwise = Nothing

  replaceLine n l b = above +++ fromString l +++ below
    where
      above = takeJ n b
      below = dropJ (n + 1) b

  numLines = getSize . snd . tag
  value = getScore . fst . tag
