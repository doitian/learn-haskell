{-# LANGUAGE UndecidableInstances #-}

module Typeclassopedia where

newtype TEither e a = TEither (Either e a) deriving (Show)

-- |
-- >>> fmap (+1) . TEither $ Left 1
-- TEither (Left 1)
-- >>> fmap (+1) . TEither $ Right 1
-- TEither (Right 2)
instance Functor (TEither e) where
  fmap g (TEither (Left x)) = TEither $ Left x
  fmap g (TEither (Right x)) = TEither . Right $ g x

newtype TFun e a = TFun {getFun :: e -> a}

-- |
-- >>> getFun (fmap (+1) $ TFun (*2)) $ 5
-- 11
instance Functor (TFun e) where
  fmap g (TFun h) = TFun $ g . h

-- |
-- >>> getFun (TFun (\x y -> (x*2):y) <*> TFun (:[])) $ 10
-- [20,10]
instance Applicative (TFun e) where
  pure = TFun . const
  (<*>) :: TFun e (a -> b) -> TFun e a -> TFun e b
  (TFun g) <*> (TFun h) = TFun $ (\x -> g x . h $ x)

newtype TTuple2 e a = TTuple2 (e, a) deriving (Show)

-- |
-- >>> fmap (+1) $ TTuple2 (True,2)
-- TTuple2 (True,3)
instance Functor (TTuple2 e) where
  fmap g (TTuple2 (x, y)) = TTuple2 $ (x, g y)

data Pair a = Pair a a deriving (Show)

-- |
-- >>> fmap (+1) $ Pair 1 2
-- Pair 2 3
instance Functor Pair where
  fmap :: (a -> b) -> Pair a -> Pair b
  fmap g (Pair x y) = Pair (g x) (g y)

data ITree a
  = Leaf (Int -> a)
  | Node [ITree a]

instance Functor ITree where
  fmap :: (a -> b) -> ITree a -> ITree b
  fmap g (Leaf h) = Leaf $ fmap g h
  fmap g (Node forest) = Node $ fmap (fmap g) forest

newtype TCompose f e a = TCompose (f (e a)) deriving (Show)

instance (Functor f, Functor e) => Functor (TCompose f e) where
  fmap :: (a -> b) -> TCompose f e a -> TCompose f e b
  fmap g (TCompose outer) = TCompose $ fmap (fmap g) outer

newtype TMaybe a = TMaybe (Maybe a) deriving (Show)

instance Functor TMaybe where
  fmap g (TMaybe (Just x)) = TMaybe . Just $ g x
  fmap _ (TMaybe Nothing) = TMaybe Nothing

-- |
-- >>> TMaybe (Just (+1)) <*> TMaybe (Just 2)
-- TMaybe (Just 3)
instance Applicative TMaybe where
  pure = TMaybe . Just

  (TMaybe (Just f)) <*> (TMaybe (Just x)) = TMaybe . Just $ f x
  _ <*> _ = TMaybe Nothing

-- |
-- >>> myApp (+) (Just 2) (Just 3)
-- Just 5
myApp :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
myApp g x y = do
  x' <- x
  y' <- y
  pure $ g x' y'

-- |
-- >>> sequenceAL [Just 4, Nothing, Just 5]
-- Nothing
--
-- >>> sequenceAL [Just 4, Just 5]
-- Just [4,5]
sequenceAL :: (Applicative f) => [f a] -> f [a]
sequenceAL = foldr concat init
  where
    concat = liftA2 (:)
    init = pure []

class (Functor f) => Monoidal f where
  unit :: f ()
  (**) :: f a -> f b -> f (a, b)

-- | Implement @pure@ and @(<*>)@ in terms of @unit@ and @(**)@, and vice versa.
instance {-# OVERLAPPABLE #-} (Functor f, Monoidal f) => Applicative f where
  pure x = fmap (const x) unit
  (<*>) :: f (a -> b) -> f a -> f b
  g <*> x = fmap (uncurry ($)) $ g Typeclassopedia.** x

data MonoidalMaybe a = MonoidalJust a | MonoidalNothing deriving (Show)

instance Functor MonoidalMaybe where
  fmap g (MonoidalJust x) = MonoidalJust $ g x
  fmap _ _ = MonoidalNothing

-- |
-- >>> (+) <$> MonoidalJust 1 <*> MonoidalJust 2
-- MonoidalJust 3
instance Monoidal MonoidalMaybe where
  unit = MonoidalJust ()
  (MonoidalJust x) ** (MonoidalJust y) = MonoidalJust (x, y)
  _ ** _ = MonoidalNothing

-- | Implement @unit@ and (**) in terms of @pure@ and @(<*>)@.
instance {-# OVERLAPPABLE #-} (Functor f, Applicative f) => Monoidal f where
  unit = pure ()
  (**) = liftA2 (,)

data ApplicativeMaybe a = ApplicativeJust a | ApplicativeNothing deriving (Show)

instance Functor ApplicativeMaybe where
  fmap g (ApplicativeJust x) = ApplicativeJust $ g x
  fmap _ _ = ApplicativeNothing

-- |
-- >>> ApplicativeJust 1 Typeclassopedia.** ApplicativeJust 2
-- ApplicativeJust (1,2)
instance Applicative ApplicativeMaybe where
  pure = ApplicativeJust
  (ApplicativeJust g) <*> (ApplicativeJust x) = ApplicativeJust $ g x
  _ <*> _ = ApplicativeNothing
