module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

data N = Z | S N
    deriving (Show)

nplus :: N -> N -> N
nplus n Z = n
nplus Z n = n
nplus (S n1) (S n2) = S $ S $ nplus n1 n2

nmult :: N -> N -> N
nmult _ Z = Z
nmult Z _ = Z
nmult (S Z) n = n
nmult n (S Z) = n
nmult (S a) b = nmult b a `nplus` b

nsub :: N -> N -> Maybe N
nsub Z Z = Just Z
nsub Z _ = Nothing
nsub (S a) Z = Just $ S a
nsub (S a) (S b) = nsub a b

ncmp :: N -> N -> Ordering
ncmp Z Z = EQ
ncmp Z _ = LT
ncmp _ Z = GT
ncmp (S a) (S b) = ncmp a b

applyNTimes :: N -> Natural -> N
applyNTimes _ 0 = Z
applyNTimes a 1 = S a
applyNTimes r n = applyNTimes (S r) (n - 1)

nFromNatural :: Natural -> N
nFromNatural = applyNTimes Z

subtractNTimes :: Num a => N -> a -> a
subtractNTimes Z a = a
subtractNTimes (S b) a = subtractNTimes b (a + 1)

nToNum :: Num a => N -> a
nToNum b = subtractNTimes b 0

nEven :: N -> Bool
nEven Z = True
nEven (S Z) = False
nEven (S (S a)) = nEven a

nOdd :: N -> Bool
nOdd a = not $ nEven a

nsubUnsafe :: N -> N -> N
nsubUnsafe Z Z = Z
nsubUnsafe Z _ = Z 
nsubUnsafe (S a) Z = S a 
nsubUnsafe (S a) (S b) = nsubUnsafe a b

nsubNTimes :: N -> N -> N -> N
nsubNTimes a Z _ = a
nsubNTimes a (S b) (S c) 
  = if ncmp (S b) (S c) == GT || ncmp (S b) (S c) == EQ
    then nsubNTimes (S a) (nsubUnsafe (S b) (S c)) (S c)
    else a
nsubNTimes _ _ _ = Z


ndiv :: N -> N -> N
ndiv _ Z = Z
ndiv Z _ = Z
ndiv (S a) (S b) = nsubNTimes Z (S a) (S b)

nmod :: N -> N -> N
nmod a b 
  = let ord = ncmp a b
    in case ord of
      GT -> nmod (nsubUnsafe a b) b
      EQ -> Z
      LT -> a

