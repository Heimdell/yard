{- |
  Various combinators.
-}
module Text.Parser.Yard.Combinators where

import Control.Applicative
import Control.Monad (void)
import Data.Set     qualified as Set
import Data.Text    qualified as Text
import Data.Char
import Data.Foldable (traverse_)

import Text.Parser.Yard.Core
import Text.Parser.Yard.Point

{- |
  Parses any char, fails at the end of stream.
-}
anyChar :: Parser Char
anyChar = satisfy \_ -> True

{- |
  Parses this exact char.
-}
char :: Char -> Parser ()
char c = do
  void do
    satisfy (== c)

{- |
  Parses this exact sequence of chars.
-}
string :: String -> Parser ()
string str = show str <?> traverse_ char str

{- |
  Return the text covered by parser, along with its result.
-}
withTrace :: Parser a -> Parser (a, Text.Text)
withTrace p = do
  start <- getPosition
  a     <- p
  end   <- getPosition
  return (a , slice start end)

{- |
  Return the text covered by parser.
-}
traceOf :: Parser a -> Parser Text.Text
traceOf p = do
  (_ , trace) <- withTrace p
  return trace

infix 1 `sepBy`, `sepBy1`

{- |
  Parse separated sequence like "1, 2, 3".
-}
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> return []

{- |
  Parse non-empty separated sequence like "1, 2, 3".
-}
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
  x  <- p
  xs <- many (sep >> p)
  return (x : xs)

{- |
  Parse one space (uses `isSpace` to decide) char.
-}
space1 :: Parser ()
space1 = void do satisfy isSpace

{- |
  Parse given space generator after the parser.
-}
mkToken :: Parser () -> Parser a -> Parser a
mkToken sp p = try p <* sp

{- |
  Parse any character from the string (uses `Set.fromList`).
-}
oneOf :: String -> Parser Char
oneOf = satisfy . flip Set.member . Set.fromList

{- |
  @foldr (\<|\>) empty@
-}
choose :: (Foldable f, Alternative t) => f (t a) -> t a
choose = foldr (<|>) empty
