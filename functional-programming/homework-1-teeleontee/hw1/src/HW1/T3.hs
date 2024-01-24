module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = (Int, Int)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf = 0  
tsize (Branch meta _ _ _) = fst meta

tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch _ l _ r) = max (tdepth l) (tdepth r) + 1

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember a (Branch _ l m r) 
  = let cmp = a `compare` m
    in case cmp of
      EQ -> True
      GT -> tmember a r
      LT -> tmember a l

mkBranch :: Tree a -> Tree a
mkBranch Leaf = Leaf
mkBranch (Branch _ l m r) 
  = Branch (tsize l + tsize r + 1, (tdepth l `max` tdepth r) + 1) l m r

leftRotate :: Tree a -> Tree a
leftRotate (Branch meta (Branch ld ll lm lr) m r) 
  = mkBranch $ Branch meta ll lm (mkBranch $ Branch ld lr m r)
leftRotate _ = Leaf

rightRotate :: Tree a -> Tree a
rightRotate (Branch meta l m (Branch rd rl rm rr)) 
  = mkBranch $ Branch meta (mkBranch $ Branch rd l m rl) rm rr
rightRotate _ = Leaf

bigLeftRotate :: Tree a -> Tree a
bigLeftRotate Leaf = Leaf
bigLeftRotate (Branch meta l m r) 
  = let lr = mkBranch $ rightRotate l
    in mkBranch $ leftRotate $ Branch meta lr m r
          
bigRightRotate :: Tree a -> Tree a
bigRightRotate Leaf = Leaf
bigRightRotate (Branch meta l m r) 
  = let rr = mkBranch $ leftRotate r
    in mkBranch $ rightRotate $ Branch meta l m rr

tcomp :: Tree a -> Bool
tcomp Leaf = True
tcomp (Branch _ l _ r) = tdepth l > tdepth r

tbalance :: Tree a -> Tree a
tbalance Leaf = Leaf
tbalance (Branch meta Leaf m r)
  = mkBranch $ case snd meta of
      0 -> Branch meta Leaf m r
      1 -> Branch meta Leaf m r
      2 -> Branch meta Leaf m r
      3 -> let cmp = tcomp r
           in if cmp 
              then bigRightRotate $ Branch meta Leaf m r
              else rightRotate $ Branch meta Leaf m r
      _ -> Leaf
tbalance (Branch meta l m Leaf)
  = mkBranch $ case snd meta of
      0 -> Branch meta l m Leaf
      1 -> Branch meta l m Leaf
      2 -> Branch meta l m Leaf
      3 -> let cmp = tcomp l
           in if cmp
              then leftRotate $ Branch meta l m Leaf
              else bigLeftRotate $ Branch meta l m Leaf
      _ -> Leaf
tbalance (Branch meta l m r) 
  = let cmp      = tdepth l - tdepth r
        absCmp   = abs cmp
    in mkBranch $ case absCmp of
        0 -> Branch meta l m r
        1 -> Branch meta l m r
        2 -> case cmp of
             2  -> if tcomp l 
                   then leftRotate $ Branch meta l m r
                   else bigLeftRotate $ Branch meta l m r
             -2 -> if tcomp r
                   then bigRightRotate $ Branch meta l m r
                   else rightRotate $ Branch meta l m r
             _  -> Leaf -- error
        _ -> Leaf -- error
                                    
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert el Leaf = Branch (1, 1) Leaf el Leaf
tinsert el (Branch meta l m r) 
  = tbalance $ let cmp = el `compare` m
               in case cmp of
                  EQ -> Branch meta l m r
                  LT -> mkBranch $ Branch meta (tinsert el l) m r
                  GT -> mkBranch $ Branch meta l m (tinsert el r)

tFromListHelper :: Ord a => [a] -> Tree a -> Tree a
tFromListHelper [] el = el
tFromListHelper xs _ = foldr tinsert Leaf xs

tFromList :: Ord a => [a] -> Tree a
tFromList xs = tFromListHelper xs Leaf

