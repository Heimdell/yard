
{- |
  Parser state.
-}
module Text.Parser.Yard.Point where

import Data.Text qualified as Text
import Data.Function (on)

{- |
  Parser position along with other junk.
-}
data Point = Point
  { before    :: String     -- ^ stack of parsed characters
  , after     :: Text.Text  -- ^ remaining unparsed text
  , line      :: Int        -- ^ line
  , col       :: Int        -- ^ column
  , offset    :: Int        -- ^ offset from the start in what haskell calls `Char`
  , filename  :: String     -- ^ name of the file being parsed
  }

instance Show Point where
  show _ = "#"

instance Eq  Point where (==)    = (==)    `on` (.offset)
instance Ord Point where compare = compare `on` (.offset)

{- |
  Move position one char ahead.
-}
nextChar :: Point -> Maybe (Char, Point)
nextChar point = do
  (c, after) <- Text.uncons point.after
  return (c, point
    { after  = after
    , before = c : point.before
    , offset = 1 + point.offset
    , line   = if c == '\n' then point.line + 1 else point.line
    , col    = if c == '\n' then 1 else point.col + 1
    })

{- |
  Grab piece of text between 2 points.
-}
slice :: Point -> Point -> Text.Text
slice start end = Text.take (end.offset - start.offset) start.after