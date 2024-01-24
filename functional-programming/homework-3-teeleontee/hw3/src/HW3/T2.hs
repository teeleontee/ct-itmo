module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  , listConcat
  ) where

import HW3.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _)        = None
distOption (_, None)        = None
distOption (Some x, Some y) = Some (x, y)

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a b, P c d) = P (a, c) (b, d)

wrapPair :: a -> Pair a
wrapPair a = P a a

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a b c d, Q e f g h) = Q (a, e) (b, f) (c, g) (d, h)

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a :# e1, b :# e2) = (a, b) :# (e1 <> e2)

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _)           = Error e
distExcept (_, Error e)           = Error e
distExcept (Success a, Success b) = Success (a, b)

wrapExcept :: a -> Except e a
wrapExcept = Success

unwrapPr :: Prioritised a -> a
unwrapPr (Low a)    = a
unwrapPr (Medium a) = a
unwrapPr (High a)   = a

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High a, b)    = High (a, unwrapPr b)
distPrioritised (a, High b)    = High (unwrapPr a, b)
distPrioritised (Medium a, b)  = Medium (a, unwrapPr b)
distPrioritised (a, Medium b)  = Medium (unwrapPr a, b)
distPrioritised (Low a, Low b) = Low (a, b)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> b, x :> y) = (a, x) :> distStream (b, y)

wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

listConcat :: List a -> List a -> List a
listConcat (a :. Nil) c = a :. c
listConcat (a :. b) c   = a :. listConcat b c
listConcat Nil a        = a

distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (a :. b, x :. y) 
  = let tmp = mapList (\i -> (a, i)) (x :. y) 
    in listConcat tmp (distList (b, x :. y))

wrapList :: a -> List a
wrapList a = a :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F fn1, F fn2) = F $ \x -> (fn1 x, fn2 x)

wrapFun :: a -> Fun i a
wrapFun a = F $ \_ -> a

