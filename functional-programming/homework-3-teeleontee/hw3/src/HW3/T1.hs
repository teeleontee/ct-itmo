module HW3.T1
  ( Option (..)
  , Pair (..)
  , Quad (..)
  , Annotated (..)
  , Except (..)
  , Prioritised (..)
  , Stream (..)
  , List (..)
  , Fun (..)
  , Tree (..)
  , mapOption
  , mapPair
  , mapQuad
  , mapAnnotated
  , mapExcept
  , mapPrioritised
  , mapStream
  , mapList
  , mapFun
  , mapTree
  ) where

data Option a = None | Some a
  deriving Show

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption _ None      = None
mapOption fn (Some a) = Some $ fn a

data Pair a = P a a
  deriving Show

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair fn (P x y) = P (fn x) (fn y)

data Quad a = Q a a a a
  deriving Show

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad fn (Q a b c d) = Q (fn a) (fn b) (fn c) (fn d)

data Annotated e a = a :# e
  deriving Show
infix 0 :#

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated fn (a :# e) = fn a :# e

data Except e a = Error e | Success a
  deriving Show

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e)    = Error e
mapExcept fn (Success a) = Success $ fn a

data Prioritised a = Low a | Medium a | High a
  deriving Show

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised fn (Low a)    = Low $ fn a
mapPrioritised fn (Medium a) = Medium $ fn a
mapPrioritised fn (High a)   = High $ fn a

data Stream a = a :> Stream a
  deriving Show
infixr 5 :>

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream fn (a :> b) = fn a :> mapStream fn b

data List a = Nil | a :. List a
  deriving Show
infixr 5 :.

mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil = Nil
mapList fn (a :. b) = fn a :. mapList fn b

data Fun i a = F (i -> a)

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun fn (F fn2) = F $ fn . fn2

data Tree a = Leaf | Branch (Tree a) a (Tree a)
  deriving Show

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf = Leaf
mapTree fn (Branch lhs val rhs) 
  = Branch (mapTree fn lhs) (fn val) (mapTree fn rhs)


