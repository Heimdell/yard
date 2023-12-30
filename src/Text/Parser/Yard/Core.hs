
{- |
  Simple parser combinators for `Text.Text` only.
-}
module Text.Parser.Yard.Core
  ( -- * Types
    Parser(..)
  , ParseError

    -- * Basic building blocks
  , satisfy
  , eof

    -- * Error reporting
  , complain

    -- * Manipulators
  , try
  , (<?>)
  , notFollowedBy

    -- * Get current state of the parser
  , getPosition
  )
  where

import Control.Monad (ap)
import Control.Applicative
import Data.Set qualified as Set

import Text.Parser.Yard.Point

{- |
  Parse error is a set of names for things parser has expected.
-}
newtype ParseError = ParseError { _err :: Set.Set String }
  deriving newtype (Semigroup, Monoid)

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

instance Show ParseError where
  show (ParseError errs) = listOf (Set.toList errs)

{- |
  Parser is a state over position in file, it returns either an error or the result.
-}
newtype Parser a = Parser
  { runParser :: Point ->
      ( Point
      , Either ParseError a
      )
  }
  deriving stock (Functor)

instance Applicative Parser where
  pure a = Parser \pos -> (pos, Right a)
  (<*>) = ap

instance Monad Parser where
  ma >>= k = Parser \pos -> do
    case ma.runParser pos of
      (mPos, Right a)   -> (k a).runParser mPos
      (mPos, Left  err) -> (mPos, Left err)

{- |
  Left-biased choice between parsers.

  This instance gives us `some`, `many` and `optional` combinators.

  Notice that `<|>` will only try right parser if the left one has consumed nothing!
-}
instance Alternative Parser where
  empty = Parser \pos -> (pos, Left (ParseError mempty))

  la <|> ra = Parser \pos -> do
    case la.runParser pos of
      (pos', Left err) | pos == pos' -> do
        case ra.runParser pos of
          (pos'', Left err1) | pos' == pos'' -> do
            (pos'', Left (err <> err1))

          res -> res

      res -> res

{- |
  Report multiple items to be expected at once.
-}
complain :: [String] -> Parser a
complain msgs = Parser \pos ->
  (pos, Left (ParseError (Set.fromList msgs)))

{- |
  Parse one char that passes the check.
-}
satisfy :: (Char -> Bool) -> Parser Char
satisfy good = Parser \pos ->
  case nextChar pos of
    Just (c, next) | good c -> do
      (next, Right c)

    _ ->
      (pos, Left mempty)

{- |
  Succeed if the end of the stream is reached.
-}
eof :: Parser ()
eof = Parser \pos ->
  case nextChar pos of
    Just _ -> (pos, Left  (ParseError (Set.singleton "end-of-file")))
    _      -> (pos, Right ())

{- |
  Dump current position - for whatever.
-}
getPosition :: Parser Point
getPosition = Parser \pos -> do
  (pos, Right pos)

{- |
  Modify the parser so if it fails, it has no effects on the state.

  Basically, enables rollback and allows parser to consume /and/ be eligible for
  `<\>`.
-}
try :: Parser a -> Parser a
try p = Parser \pos -> do
  case p.runParser pos of
    (_, Left err) -> (pos, Left err)
    res           -> res

{- |
  Modify the parser so if it fails while consuming nothing, its errors are replaced
  with new one.
-}
(<?>) :: String -> Parser a -> Parser a
msg <?> p = Parser \pos ->
  case p.runParser pos of
    (pos', Left _) | pos == pos' -> (pos', Left (ParseError (Set.singleton msg)))
    res                          -> res

{- |
  Check that parser fails. Ignore any side effects.
-}
notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = Parser \pos ->
  case p.runParser pos of
    (_ , Right _) -> (pos, Left mempty)
    (_ , Left  _) -> (pos, Right ())
