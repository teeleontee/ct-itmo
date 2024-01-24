module HW2.T3
  ( epart
  , mcat
  ) where

fromMaybe :: Monoid a => Maybe a -> a
fromMaybe a 
  = case a of
      Just x  -> x
      Nothing -> mempty

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap fromMaybe

toMonoidPair :: (Monoid a, Monoid b) => Either a b -> (a, b)
toMonoidPair a =
  case a of
    Left x  -> (x, mempty)
    Right y -> (mempty, y)

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap toMonoidPair

