module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty as NonEmpty

splitOnHelper :: Eq a => a -> [a] -> [a] -> [[a]] -> [[a]]
splitOnHelper _ [] cur final = final ++ [cur]
splitOnHelper del (x:xs) cur final
  | del == x  = splitOnHelper del xs [] (final ++ [cur])
  | otherwise = splitOnHelper del xs (cur ++ [x]) final

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sym arr
  = let ans = splitOnHelper sym arr [] []
    in case ans of
      (x:xs) -> x :| xs
      []     -> [] :| []

joinWithHelper :: a -> [a] -> [a] -> [a]
joinWithHelper _ [] acc = acc
joinWithHelper sep (x:xs) acc
  = joinWithHelper sep xs (acc ++ [x])

flat :: a -> [[a]] -> [a]
flat _ [] = []
flat _ [x] = x
flat sep (x:xs) 
  = let fl = flat sep xs 
    in x ++ [sep] ++ fl

joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ (x :| []) = x
joinWith sep (x :| xs) 
  = let lst = flat sep xs
    in joinWithHelper sep (x ++ [sep] ++ lst) []

