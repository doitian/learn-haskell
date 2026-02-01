module JoinList
  ( JoinList (..),
    (+++),
    tag,
    indexJ,
    dropJ,
    takeJ,
  )
where

import Sized

-- |
-- >>> Single 1 "x"
-- Single 1 "x"
data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- |
-- >>> Single "a" 1 +++ Single "b" 1
-- Append "ab" (Single "a" 1) (Single "b" 1)
(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ y = y
x +++ Empty = x
x +++ y = Append (tag x <> tag y) x y

-- |
-- >>> tag (Empty :: JoinList [Int] Int)
-- []
-- >>> tag $ Single [1] 0
-- [1]
tag :: (Monoid m) => JoinList m a -> m
tag Empty = mempty
tag (Single t _) = t
tag (Append t _ _) = t

bsearch :: (Monoid b) => (b -> Bool) -> JoinList b a -> Maybe a
bsearch _ Empty = Nothing
bsearch p jl
  | p $ tag jl = go mempty jl
  | otherwise = Nothing
  where
    -- go is a version of bsearch that does not check the tag of the current node.
    -- The first argument is the accumulated tag for skipped nodes from the left
    go _ Empty = Nothing
    go _ (Single _ x) = Just x
    go skipped (Append _ xs ys)
      | p $ tag xs = go skipped xs
      | otherwise = go (skipped <> tag xs) ys

-- $setup
-- >>> let jl = Single (Size 1) 'a' +++ Single (Size 1) 'b' +++ Single (Size 1) 'c'

-- |
-- >>> indexJ 0 jl
-- Just 'a'
-- >>> indexJ 2 jl
-- Just 'c'
-- >>> indexJ 3 jl
-- Nothing
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i = bsearch $ (i <) . getSize . size

-- |
-- >>> dropJ 0 jl == jl
-- True
-- >>> dropJ 1 jl == Single (Size 1) 'b' +++ Single (Size 1) 'c'
-- True
-- >>> dropJ 2 jl == Single (Size 1) 'c'
-- True
-- >>> dropJ 3 jl
-- Empty
-- >>> dropJ 4 jl
-- Empty
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl
  | n <= 0 = jl
dropJ n (Single _ _) = Empty
dropJ n (Append t xs ys)
  | tSize <= n = Empty
  | n <= xsSize = dropJ n xs +++ ys
  | otherwise = dropJ (n - xsSize) ys
  where
    tSize = getSize . size $ t
    xsSize = getSize . size . tag $ xs

-- |
-- >>> takeJ 0 jl
-- Empty
-- >>> takeJ 1 jl == Single (Size 1) 'a'
-- True
-- >>> takeJ 2 jl == Single (Size 1) 'a' +++ Single (Size 1) 'b'
-- True
-- >>> takeJ 3 jl == jl
-- True
-- >>> takeJ 4 jl == jl
-- True
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl
  | n <= 0 = Empty
takeJ n jl@(Single _ _) = jl
takeJ n jl@(Append t xs ys)
  | tSize <= n = jl
  | n <= xsSize = takeJ n xs
  | otherwise = xs +++ takeJ (n - xsSize) ys
  where
    tSize = getSize . size $ t
    xsSize = getSize . size . tag $ xs
