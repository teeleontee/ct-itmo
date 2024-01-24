{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module HW6.T2
  ( TSet
  , Contains
  , Add
  , Delete
  ) where

import GHC.TypeLits
import Data.Type.Bool

-- | A list of type-level strings
type TSet = [Symbol]

-- | Checks whether we contain a type-level string
type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains name '[]       = 'False
  Contains name (name:xs) = 'True
  Contains name (x:xs)    = Contains name xs

-- | Adds a type-level string to the set if it isn't in already there
type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add value set = If (Contains value set) set (value:set)

-- | Deletes a type-level string if it contains it
type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete _ '[]            = '[]
  Delete value (value:xs) = xs
  Delete value (x:xs)     = x : Delete value xs

