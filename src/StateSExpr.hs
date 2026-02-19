module StateSExpr where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Data.Char

type Parser a = StateT String Maybe a

runParser :: (Parser a) -> String -> Maybe (a, String)
runParser = runStateT

-- |
-- >>> runParser (satisfy (== 'x')) "xy"
-- Just ('x',"y")
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  text <- get
  case text of
    (x : xs) | p x -> put xs >> return x
    otherwise -> mzero

char :: Char -> Parser Char
char ch = satisfy (== ch)

-- |
-- >>> runParser integer "-12"
-- Just (-12,"")
integer :: Parser Integer
integer = do
  op <- maybe id (const negate) <$> optional (char '-')
  num <- read <$> some (satisfy isDigit)
  return $ op num

spaces :: Parser String
spaces = many $ satisfy isSpace

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
ident = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
