
{- |
  Different ways to run a parser.
-}
module Text.Parser.Yard.Run where

import Data.Text    qualified as Text
import Data.Text.IO qualified as Text

import Text.Parser.Yard.Core
import Text.Parser.Yard.Report
import Text.Parser.Yard.Point

{- |
  Most general way, you privde a parser, a filename (for report) and text to parse.
-}
parse :: Parser a -> String -> Text.Text -> Either (Report ParseError) a
parse p filename after = do
  let start = Point {after, before = [], col = 1, line = 1, filename, offset = 0}
  case p.runParser start of
    (mPos, Left  errs) -> Left Report {point = mPos, errs}
    (_,    Right a)    -> Right a

{- |
  For testing. You provide a parser and a text to parse (as string).

  The filename is set to "\<stdin\>".
-}
parseTest :: Show a => Parser a -> String -> IO ()
parseTest p src = do
  either print print (parse p "<stdin>" (Text.pack src))

{- |
  Read the file and parse it as whole.
-}
parseFile :: Show a => Parser a -> String -> IO (Either (Report ParseError) a)
parseFile p filename = do
  src <- Text.readFile filename
  return (parse p filename src)
