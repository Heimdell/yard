{- |
  Pretty (I hope) report generator.
-}
module Text.Parser.Yard.Report where

import Data.Set  qualified as Set
import Data.Text qualified as Text

import Text.Parser.Yard.Point

{- |
  Information to report back to user in case of error.
-}
data Report = Report
  { point :: Point           -- ^ where?
  , errs  :: Set.Set String  -- ^ what items were expected?
  }

{- |
  Punctuate with commas and an "or" between last two items.
-}
listOf :: [String] -> String
listOf [] = "unexpected"
listOf (a' : xs') = "expected " <> listOf' a' xs'
  where
    listOf' :: String -> [String] -> String
    listOf' a []       = a
    listOf' a [b]      = a <> " or " <> b
    listOf' a (b : xs) = a <> ", "   <> listOf' b xs

{- |
  Convert report to a pretty string.
-}
instance Show Report where
  show report = unlines
    [ prefix <> srcLine
    , prefix <> cursor
    , prefix <> expected
    ]
    where
      Point {line, col, before, after} = report.point

      prefix = report.point.filename <> ":" <> show line <> ":" <> show col <> "> "

      srcLine = reverse (takeWhile (/= '\n') before) <> Text.unpack (Text.takeWhile (/= '\n') after)
      cursor  = replicate (col - 1) ' ' <> "^"

      expected = listOf (Set.toList report.errs)