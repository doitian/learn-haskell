module Hanoi where

type Peg = String

type Move = (Peg, Peg)

-- |
-- >>> length (hanoi 15 "a" "b" "c")
-- 32767
--
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"),("a","b"),("c","b")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

-- |
-- >>> length (hanoi4 15 "a" "b" "c" "d")
-- 129
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 2 a b c _ = [(a, c), (a, b), (c, b)]
hanoi4 3 a b c d = [(a, c), (a, d), (a, b), (d, b), (c, b)]
hanoi4 n a b c d =
  let k = (floor . sqrt . fromIntegral) (2 * n)
   in hanoi4 (n - k) a d b c ++ hanoi k a b c ++ hanoi4 (n - k) d b a c
