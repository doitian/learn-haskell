module Calc where

import Data.Char
import Parser

data ExprT
  = Lit Integer
  | Add ExprT ExprT
  | Mul ExprT ExprT
  deriving (Show, Eq)

-- | >>> eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
-- 20
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- | >>> evalStr "(2+3)*4"
-- Just 20
-- >>> evalStr "2+3*4"
-- Just 14
-- >>> evalStr "2+3*"
-- Nothing
evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
  Just expr -> Just (eval expr)
  Nothing -> Nothing

class Expr a where
  lit :: Integer -> a
  add, mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit value = value > 0
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7
