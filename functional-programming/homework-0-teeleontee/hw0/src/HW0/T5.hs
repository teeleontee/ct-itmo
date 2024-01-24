module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ x = x

ns :: Nat a -> Nat a
ns n f a = n f (f a)

nplus :: Nat a -> Nat a -> Nat a
nplus a1 a2 f s = a1 f (a2 f s)

nmult :: Nat a -> Nat a -> Nat a
nmult f g h = f $ g h

nFromNatural :: Natural -> Nat a
nFromNatural n | n == 0    = nz
               | otherwise = ns $ nFromNatural $ n - 1

nToNum :: Num a => Nat a -> a
nToNum n = n (+1) 0

