module Homework4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

-- |
-- >>> fun1 [] == fun1' []
-- True
-- >>> fun1 [5] == fun1' [5]
-- True
-- >>> fun1 [3,5] == fun1' [3,5]
-- True
fun1' :: [Integer] -> Integer
fun1' = foldl' (*) 1 . map (`subtract` 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-- |
-- >>> fun2 1 == fun2' 1
-- True
-- >>> fun2 5 == fun2' 5
-- True
-- >>> fun2 10 == fun2' 10
-- True
fun2' :: Integer -> Integer
fun2' = sum . takeWhile (/= 0) . filter even . iterate next
  where
    next 1 = 0
    next n
      | even n = n `div` 2
      | otherwise = 3 * n + 1

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

-- >>> :{
-- foldTree "ABCDEFGHIJ"
--   == Node
--     3
--     ( Node
--         2
--         (Node 1 (Node 0 Leaf 'D' Leaf) 'G' Leaf)
--         'I'
--         (Node 1 (Node 0 Leaf 'A' Leaf) 'E' Leaf)
--     )
--     'J'
--     ( Node
--         2
--         (Node 1 (Node 0 Leaf 'B' Leaf) 'F' Leaf)
--         'H'
--         (Node 0 Leaf 'C' Leaf)
--     )
-- :}
-- True
foldTree :: (Foldable t) => t a -> Tree a
foldTree = foldr insert Leaf
  where
    insert value Leaf = Node 0 Leaf value Leaf
    insert value (Node h left root right)
      | height left > height right = Node h left root (insert value right)
      -- We always insert to left first, so height left must equal to right now
      | otherwise = Node (1 + height newLeft) newLeft root right
      where
        newLeft = insert value left

-- |
-- >>> map xor [[], [False, True, False], [False, True, False, False, True]]
-- [False,True,False]
xor :: (Foldable t) => t Bool -> Bool
xor = foldl' xor' False
  where
    xor' True y = not y
    xor' False y = y

-- |
-- >>> map' (1+) [1,2,3]
-- [2,3,4]
--
-- >>> map' id []
-- []
map' :: (a -> b) -> [a] -> [b]
map' f = foldr step []
  where
    step x acc = f x : acc

-- |
-- >>> myFoldl (-) 10 [1, 2, 3]
-- 4
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f acc xs = foldr (flip f) acc (reverse xs)

-- |
-- >>> sieveSundaram 5
-- [3,5,7,11]
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = oddPrimeNumbers
  where
    -- The sieve starts with a list of the integers from 1 to n.
    initial = [1 .. n]
    -- From this list, all numbers of the form i + j + 2ij are removed,
    -- where i and j are positive integers such that 1 ≤ i ≤ j and i + j + 2ij ≤ n.
    exclusions = [i + j + 2 * i * j | j <- [1 .. (n - 1) `div` 3], i <- [1 .. j]]
    remaining = filter (`notElem` exclusions) initial
    -- The remaining numbers are doubled and incremented by one, giving a list of the odd prime numbers (that is, all primes except 2) below 2n + 2.
    oddPrimeNumbers = map (\x -> 2 * x + 1) remaining
