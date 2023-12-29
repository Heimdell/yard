
{- |
  Machinery to parse operator expressions (only right-associative ones).
-}
module Text.Parser.Yard.Expr where

import Control.Applicative

import Text.Parser.Yard.Core
import Text.Parser.Yard.Combinators

{- |
  Expression tree.
-}
data Expr op a
  = Atom a
  | Op   { left :: Expr op a, op :: op , right :: Expr op a }

instance (Show op, Show a) => Show (Expr op a) where
  show = \case
    Atom a -> show a
    Op l o r -> "(" ++ show l ++ " " ++ show o ++ " " ++ show r ++ ")"

{- |
  Expression parser generator.
-}
opTable :: forall op atom. [[Parser op]] -> Parser atom -> Parser (Expr op atom)
opTable ops' atom = go ops'
  where
    go :: [[Parser op]] -> Parser (Expr op atom)
    go []           = Atom <$> atom
    go (ops : rest') = do
      left  <- go rest'
      rest  <- optional do
        op    <- choose ops
        right <- go (ops : rest')
        return (op, right)
      return $ foldl (\l (op, r) -> Op {left = l, op, right = r}) left rest