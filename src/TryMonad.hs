module TryMonad where

import Data.Monoid (Endo (..))

-- | >>> join $ Just (Just 1)
-- Just 1
join :: (Monad m) => m (m a) -> m a
join = (>>= id)

-- | >>> liftM (+1) $ Just 1
-- Just 2
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM g x = x >>= return . g

-- | >>> ap (Just (+1)) (Just 1)
-- Just 2
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mg mx = mg >>= (\g -> mx >>= (\x -> return $ g x))

-- | >>> msequence [Just 1, Just 2]
-- Just [1,2]
msequence :: (Monad m) => [m a] -> m [a]
msequence = foldr (ap . liftM (:)) (return [])

-- | >>> msequence' [Just 1, Just 2]
-- Just [1,2]
msequence' :: (Monad m) => [m a] -> m [a]
msequence' xs = (appEndo . mconcat . map endo $ xs) $ return []
  where
    endo x = Endo $ ap (liftM (:) x)

-- | Simply a combination of replicate and sequence.
-- >>> replicateM 3 (Just 1)
-- Just [1,1,1]
replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM n = liftM (replicate n)

-- | Maps its first argument over the second, and sequences the results. The
-- forM function is just mapM with its arguments reversed; it is called forM
-- since it models generalized for loops: the list @[a]@ provides the loop
-- indices, and the function @a -> m b@ specifies the “body” of the loop for
-- each index. Again, these functions actually work over any Traversable, not
-- just lists, and they can also be defined in terms of Applicative, not Monad:
-- the analogue of mapM for Applicative is called traverse.
-- >>> mapM' (:[]) [1,2]
-- [[1,2]]
mapM' :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM' = (.) msequence . map

-- | Just (>>=) with its arguments reversed; sometimes this direction is more convenient since it corresponds more closely to function application.
-- >>> (\x -> Just $ x + 1) =<<. Just 1
-- Just 2
(=<<.) :: (Monad m) => (a -> m b) -> m a -> m b
(=<<.) = flip (>>=)

-- | Sort of like function composition, but with an extra m on the result type
-- of each function, and the arguments swapped. We’ll have more to say about
-- this operation later. There is also a flipped variant, @(<=<)@.
--
-- Like flipped @(.)@, or @(>>>)@ in "Control.Category".
--
-- >>> (\x -> Just $ x + 1) >=>. (\x -> Just $ x * 2) $ 1
-- Just 4
(>=>.) :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
-- g >=>. h = \a -> g a >>= h
g >=>. h = \a -> do
  b <- g a
  c <- h b
  return c
