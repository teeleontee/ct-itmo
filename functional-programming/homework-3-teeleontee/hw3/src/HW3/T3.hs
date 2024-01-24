module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1

import HW3.T2 (listConcat)

joinOption :: Option (Option a) -> Option a
joinOption None = None
joinOption (Some x) = x

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e) = Error e
joinExcept (Success a) = a

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e1) :# e2) = a :# (e2 <> e1) 

joinList :: List (List a) -> List a
joinList Nil = Nil
joinList (a :. b) = listConcat a (joinList b)

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F fn) = F $ \x -> unwrapFun (fn x) x
                   where unwrapFun (F f) = f
                 
