module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
  deriving Show
infixr 5 :+

instance Semigroup (ListPlus a) where
  Last x <> y = x :+ y
  (x :+ xs) <> y = x :+ xs <> y

data Inclusive a b = This a | That b | Both a b
  deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  This a <> This b     = This (a <> b)
  This a <> That b     = Both a b
  This a <> Both x y   = Both (a <> x) y
  That a <> That b     = That (a <> b)
  That a <> This b     = Both b a
  That a <> Both x y   = Both x (a <> y)
  Both x y <> This z   = Both (x <> z) y
  Both x y <> That z   = Both x (y <> z)
  Both x y <> Both a b = Both (x <> a) (y <> b)
  

newtype DotString = DS String
  deriving Show

instance Semigroup DotString where
  DS a <> DS ""   = DS a
  DS "" <> DS a   = DS a
  DS a <> DS b = DS $ a ++ "." ++ b

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  F f <> F g = F $ f . g

instance Monoid (Fun a) where
  mempty = F id

