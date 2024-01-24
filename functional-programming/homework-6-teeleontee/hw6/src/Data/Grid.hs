-- | This module defines 'Grid' datatype.
module Data.Grid
  ( Grid (..)
  , extract
  , left
  , right
  , up
  , down
  , gridRead
  , gridWrite
  , horizontal
  , vertical
  ) where

import Control.Comonad (Comonad (..))

import Data.ListZipper

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

up, down :: Grid a -> Grid a
up   (Grid g) = Grid (listLeft  g)
down (Grid g) = Grid (listRight g)  

left, right :: Grid a -> Grid a
left  (Grid g) = Grid (fmap listLeft  g)
right (Grid g) = Grid (fmap listRight g)

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g
 
gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal, vertical :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left right
vertical   = genericMove up   down

instance Functor Grid where
  fmap f = Grid . (fmap . fmap) f . unGrid

instance Comonad Grid where
    extract = gridRead
    duplicate = Grid . fmap horizontal . vertical
