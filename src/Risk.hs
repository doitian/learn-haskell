{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.Bifunctor (first)
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
  deriving (Eq, Ord, Show, Num)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield {attackers :: Army, defenders :: Army}

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d) = do
  let as = min 3 (a - 1)
      ds = min 2 d
  as' <- sort <$> sequence (replicate as die)
  ds' <- sort <$> sequence (replicate ds die)
  let pairs = zip as' ds'
      losses = length $ filter (uncurry (>)) pairs
  return $ Battlefield (a - losses) (d - (length pairs - losses))

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = do
  bf' <- battle bf
  if defenders bf' == 0 || attackers bf' < 2
    then return bf'
    else invade bf'

isSuccess :: Battlefield -> Bool
isSuccess bf = defenders bf == 0

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  trials <- sequence (replicate 1000 (invade bf))
  let successes = length $ filter isSuccess trials
  return $ fromIntegral successes / 1000
