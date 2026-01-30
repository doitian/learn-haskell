{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

-- | >>> [fib 1, fib 2, fib 5]
-- [1,1,5]
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- | >>> take 5 fibs1
-- [0,1,1,2,3]
fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- | >>> take 5 fibs2
-- [0,1,1,2,3]
fibs2 :: [Integer]
fibs2 = map fst . iterate next $ (0, 1)
  where
    next (a, b) = (b, a + b)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamTake :: Int -> Stream a -> [a]
streamTake n = take n . streamToList

-- | >>> streamTake 3 . streamRepeat $ 5
-- [5,5,5]
streamRepeat :: a -> Stream a
streamRepeat = Cons <*> streamRepeat

-- | >>> streamTake 3 . streamMap (1+) . streamRepeat $ 5
-- [6,6,6]
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

instance Functor Stream where
  fmap = streamMap

instance Applicative Stream where
  pure = streamRepeat

  -- Pairwise applicative
  (Cons f fs) <*> (Cons x xs) = Cons (f x) $ (fs <*> xs)

-- | >>> streamTake 3 . streamFromSeed (*2) $ 1
-- [1,2,4]
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed . streamFromSeed f $ f seed

-- | >>> streamTake 3 nats
-- [0,1,2]
nats :: Stream Integer
nats = streamFromSeed (1 +) 0

-- | >>> streamTake 6 . interleaveStreams nats $ streamRepeat 0
-- [0,0,1,0,2,0]
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) y = Cons x $ interleaveStreams y xs

-- |
-- >>> let check (i, p) = odd (i `div` p) && i `mod` p == 0
-- >>> foldl1 (&&) . map check . zip [1..32] . map (2^) . streamTake 32 $ ruler
-- True
--
-- >>> streamTake 8 ruler
-- [0,1,0,2,0,1,0,3]
ruler :: Stream Integer
ruler = interleave nats
  where
    interleave (Cons x xs) = interleaveStreams (streamRepeat x) (interleave xs)

x :: Stream Integer
x = Cons 0 . Cons 1 $ streamRepeat 0

-- |
-- >>> streamTake 3 (fromInteger 5 :: Stream Integer)
-- [5,0,0]
--
-- >>> streamTake 3 . negate $ nats
-- [0,-1,-2]
--
-- >>> streamTake 3 $ nats + x
-- [0,2,2]
--
-- >>> streamTake 10 $ x ^ 4
-- [0,0,0,0,1,0,0,0,0,0]
--
-- >>> streamTake 10 $ (1 + x)^5
-- [1,5,10,10,5,1,0,0,0,0]
--
-- >>> streamTake 10 $ (x^2 + x + 3) * (x - 5)
-- [-15,-2,-4,1,0,0,0,0,0,0]
instance Num (Stream Integer) where
  fromInteger n = Cons n $ streamRepeat 0

  negate = streamMap negate

  (+) = liftA2 (+)

  (Cons a as) * (Cons b bs) = Cons (a * b) $ (fromInteger a) * bs + as * (Cons b bs)

-- |
-- >>> streamTake 5 $ (x^2 + x + 3) * (x - 5) / (x^2 + x + 3)
-- [-5,1,0,0,0]
--
-- >>> streamTake 5 $ (x^2 + x + 3) * (x - 5) / (x - 5)
-- [3,1,1,0,0]
instance Fractional (Stream Integer) where
  (Cons a as) / (Cons b bs) = q
    where
      q = Cons (a `div` b) $ streamMap (`div` b) (as - q * bs)

-- | >>> streamTake 9 fibs3
-- [0,1,1,2,3,5,8,13,21]
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

data Matrix2x2 = Matrix2x2 Integer Integer Integer Integer
  deriving (Eq, Show)

-- | >>> Matrix2x2 1 1 1 0 ^ 2
-- Matrix2x2 2 1 1 1
instance Num Matrix2x2 where
  (Matrix2x2 a00 a01 a10 a11) * (Matrix2x2 b00 b01 b10 b11) =
    Matrix2x2
      (a00 * b00 + a01 * b10)
      (a00 * b01 + a01 * b11)
      (a10 * b00 + a11 * b10)
      (a10 * b01 + a11 * b11)

-- | >>> map fibs4 [0..8]
-- [0,1,1,2,3,5,8,13,21]
fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 1 = 1
fibs4 n = a00
  where
    f = Matrix2x2 1 1 1 0
    (Matrix2x2 a00 _ _ _) = f ^ (n - 1)
