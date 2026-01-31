{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import qualified Data.Map as M
import Parser
import qualified StackVM

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
evalStr = fmap eval . parseExp Lit Add Mul

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

instance Expr StackVM.Program where
  lit value = [StackVM.PushI value]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

-- |
-- >>> fmap StackVM.stackVM . compile $ "(3 * -4) + 5"
-- Just (Right (IVal (-7)))
--
-- >>> compile $ "(3 * -4) + 5"
-- Just [PushI 3,PushI (-4),Mul,PushI 5,Add]
compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

class HasVars a where
  var :: String -> a

-- |
-- >>> add (lit 3) (var "x") :: VarExprT
-- VarAdd (VarLit 3) (Var "x")
data VarExprT
  = VarLit Integer
  | VarAdd VarExprT VarExprT
  | VarMul VarExprT VarExprT
  | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VarLit
  add = VarAdd
  mul = VarMul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add = liftA2 $ liftA2 (+)
  mul = liftA2 $ liftA2 (*)

-- |
-- >>> withVars [("x", 6)] $ add (lit 3) (var "x")
-- Just 9
-- >>> withVars [("x", 6)] $ add (lit 3) (var "y")
-- Nothing
-- >>> withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
-- Just 54
withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
