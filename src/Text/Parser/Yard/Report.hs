{- |
  Pretty (I hope) report generator.
-}
module Text.Parser.Yard.Report where

import Data.Text qualified as Text

import Text.Parser.Yard.Point

{- |
  Information to report back to user in case of error.
-}
data Report a = Report
  { point :: Point           -- ^ where?
  , errs  :: a  -- ^ what items were expected?
  }

{- |
  Convert report to a pretty string.
-}
instance Show a => Show (Report a) where
  show report = unlines
    [ prefix <> srcLine
    , prefix <> cursor
    , prefix <> show report.errs
    ]
    where
      Point {line, col, before, after} = report.point

      prefix = report.point.filename <> ":" <> show line <> ":" <> show col <> "> "

      srcLine = reverse (takeWhile (/= '\n') before) <> Text.unpack (Text.takeWhile (/= '\n') after)
      cursor  = replicate (col - 1) ' ' <> "^"
