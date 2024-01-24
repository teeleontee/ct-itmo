module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Numeric.Natural (Natural)
import Data.Function (fix)


repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' = fix (\rec f list -> case list of
                             x:xs -> f x : rec f xs
                             []    -> [])

fib :: Natural -> Natural
fib = fix (\rec pr pr2 n -> if n > 0
                            then rec pr2 (pr + pr2) (n - 1)
                            else pr) 0 1 


fac :: Natural -> Natural
fac = fix (\f pr pow -> if pow /= 0
                        then f (pr * pow) (pow - 1)
                        else pr) 1
