module AParser where

import Control.Applicative
import Data.Bifunctor (first)
import Data.Char
import Data.Functor

-- A parser for a value of type a is a function which takes a String
-- representing the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing -- fail on the empty input
    f (x : xs) -- check if x satisfies the predicate
    -- if so, return x along with the remainder
    -- of the input (that is, xs)
      | p x = Just (x, xs)
      | otherwise = Nothing -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

\*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
\*Parser> runParser (satisfy isUpper) "abc"
Nothing
\*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- |
-- >>> runParser (fmap toUpper (char 'x')) "xyz"
-- Just ('X',"yz")
instance Functor Parser where
  fmap g (Parser p) = Parser $ \s -> first g <$> p s

-- |
-- >>> runParser ((,) <$> char 'x' <*> char 'y') "xyz"
-- Just (('x','y'),"z")
instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)

  Parser pf <*> Parser pa = Parser $ \s -> do
    (f, s') <- pf s
    (a, s'') <- pa s'
    pure (f a, s'')

-- |
-- >>> runParser intPair "12 34"
-- Just ([12,34],"")
intPair :: Parser [Integer]
intPair = (\a b -> [a, b]) <$> posInt <* char ' ' <*> posInt

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser x <|> Parser y = Parser $ \s -> maybe (y s) Just (x s)

-- |
-- >>> runParser intOrUppercase "345abcd"
-- Just ((),"abcd")
-- >>> runParser intOrUppercase "XYZ"
-- Just ((),"YZ")
-- >>> runParser intOrUppercase "foo"
-- Nothing
intOrUppercase :: Parser ()
intOrUppercase = doPosInt <|> doSatisfyIsUpper
  where
    doPosInt = posInt $> ()
    doSatisfyIsUpper = satisfy isUpper $> ()
