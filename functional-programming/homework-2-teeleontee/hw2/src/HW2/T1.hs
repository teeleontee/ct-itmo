module HW2.T1
  ( Tree (..)
  , tfoldr
  ) where

data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ el Leaf = el 
tfoldr fn el (Branch _ lhs val rhs) 
  = let rightApplied = tfoldr fn el rhs
        applyValue   = fn val rightApplied
    in tfoldr fn applyValue lhs

