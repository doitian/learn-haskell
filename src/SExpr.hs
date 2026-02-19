{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char
import Data.Functor

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- |
-- >>> runParser (zeroOrMore (char 'x')) "xxy"
-- Just ("xx","y")
-- >>> runParser (zeroOrMore (char 'x')) "yyx"
-- Just ("","yyx")
zeroOrMore :: (Alternative f) => f a -> f [a]
zeroOrMore p = oneOrMore p <|> pure []

-- |
-- >>> runParser (oneOrMore (char 'x')) "xxy"
-- Just ("xx","y")
-- >>> runParser (oneOrMore (char 'x')) "yyx"
-- Nothing
oneOrMore :: (Alternative f) => f a -> f [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

-- |
-- >>> runParser spaces "a"
-- Just ("","a")
-- >>> runParser spaces "  a"
-- Just ("  ","a")
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

-- |
-- >>> runParser ident "foobar baz"
-- Just ("foobar"," baz")
-- >>> runParser ident "foo33fA"
-- Just ("foo33fA","")
-- >>> runParser ident "2ab"
-- Nothing
-- >>> runParser ident ""
-- Nothing
ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving (Show)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr
  = A Atom
  | Comb [SExpr]
  deriving (Show)

-- |
-- >>> runParser parseSExpr "345"
-- Just (A (N 345),"")
-- >>> runParser parseSExpr "abc"
-- Just (A (I "abc"),"")
-- >>> runParser parseSExpr "(  abc 1)"
-- Just (Comb [A (I "abc"),A (N 1)],"")
parseSExpr :: Parser SExpr
parseSExpr = spaces *> sexpr <* spaces
  where
    sexpr = atom <|> comb
    atom = fmap N posInt <|> fmap I ident <&> A
    comb = char '(' *> (zeroOrMore parseSExpr <&> Comb) <* char ')'
