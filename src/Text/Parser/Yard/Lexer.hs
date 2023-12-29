
{- |
  Prepacks of some useful stuff for parsing languages.
-}
module Text.Parser.Yard.Lexer where

import Control.Applicative
import Control.Monad (void, guard)
import Data.Map     qualified as Map
import Data.Text    qualified as Text
import Data.Maybe (isJust)
import Data.Char

import Text.Parser.Yard.Core
import Text.Parser.Yard.Combinators

{- |
  Parse line comment, starting from given sigil.
-}
lineComment :: String -> Parser ()
lineComment start = do
  try (string start)
  _ <- many do
    notFollowedBy (char '\n')
    anyChar
  pure ()

{- |
  Parse recursive block comment, delimited by given sigils.
-}
blockComment :: String -> String -> Parser ()
blockComment start end = do
  try (string start)
  _ <- many $ do
      notFollowedBy (string start)
      notFollowedBy (string end)
      void anyChar
    <|>
      blockComment start end
  try (string end)
  pure ()

{- |
  Parse body of single char literal (with escapes).
-}
charLiteral :: Map.Map Char Char -> Parser Char
charLiteral chars = do
    char '\\'
    try do
      k <- anyChar
      escape k
  <|>
    anyChar
  where
    escape :: Char -> Parser Char
    escape c = case Map.lookup c chars of
      Just res -> return res
      Nothing  -> complain (map show (Map.keys chars))

{- |
  Parse string literal body. Final sigil is passed in, so it won't be consumed.

  You can also specify character escapes, but only \\X-like constructs are supported
  at the moment.
-}
stringLiteral :: Map.Map Char Char -> String -> Parser String
stringLiteral escape quote = do
  res <- many do
    notFollowedBy (string quote)
    charLiteral escape
  return res

{- |
  Default codes for chars that can be escaped.
-}
defaultCharEscapes :: Map.Map Char Char
defaultCharEscapes = Map.fromList
  [ ('n'  , '\n')
  , ('t'  , '\t')
  , ('"'  , '\"')
  , ('\'' , '\'')
  ]

{- |
  Fractional or decimal literal. Only supports `Double` precision, no
  @scientific@.
-}
number :: Parser (Either Double Integer)
number = "number" <?> do
  (float, tracing) <- withTrace do
    _ <- try do
      _ <- optional (oneOf "-")
      some (satisfy isNumber)

    frac <- optional do
      char '.'
      _ <- "fractional part" <?> do
        some (satisfy isNumber)

      optional do
        _ <- oneOf "Ee"
        _ <- optional do oneOf "+-"

        "exponent part" <?> do
          some (satisfy isNumber)

    pure (isJust frac)

  if float
  then return (Left  (read (Text.unpack tracing)))
  else return (Right (read (Text.unpack tracing)))

{- |
  Generate name parser. Supports names like @is-type-of-Point-decidable?@

  Arguments:

      * first chars

      * fill chars

      * slug-separator

      * isReserved
-}
mkName :: Parser Char -> Parser Char -> Parser Char -> (Text.Text -> Bool) -> Parser Text.Text
mkName firstChar sep nameChar isReserved = try do
  res <- "name" <?> do
    traceOf do
      _ <- firstChar
      _ <- many nameChar
      many do
        _ <- "name separator" <?> sep
        "name part"      <?> some nameChar

  "non-reserved/non-operator name" <?> guard (not (isReserved res))
  return res

{- |
  Kebab-lowercase preset name.
-}
kebabLowerCase :: (Text.Text -> Bool) -> Parser Text.Text
kebabLowerCase isReserved = "lowercase name" <?> mkName
  do oneOf ['a'.. 'z']
  do oneOf "-"
  do oneOf do ['a'.. 'z'] ++ ['A'.. 'Z'] ++ ['0'.. '9'] ++ "!?"
  do isReserved

{- |
  Kebab-uppercase preset name.
-}
kebabUpperCase :: (Text.Text -> Bool) -> Parser Text.Text
kebabUpperCase isReserved = "uppercase name" <?> mkName
  do oneOf ['A'.. 'Z']
  do oneOf "-"
  do oneOf do ['a'.. 'z'] ++ ['A'.. 'Z'] ++ ['0'.. '9'] ++ "!?"
  do isReserved