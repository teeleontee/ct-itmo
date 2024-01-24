{-# LANGUAGE  ConstraintKinds #-}
module HW6.T1
  ( BucketsArray
  , CHT (..)
  , newCHT
  , getCHT
  , putCHT
  , sizeCHT
  , initCapacity
  , loadFactor
  ) where

import Control.Concurrent.Classy 

import Data.Array.MArray
import Data.Array.Base
import Data.Hashable
import Control.Monad

-- | Type constraint for keys in our CHT
type Key a = (Eq a, Hashable a)

-- | Initial capacity of CHT
initCapacity :: Int
initCapacity = 16

-- | This is the threshold for when we need to 
-- update the capacity of our CHT. 
-- chtSize >= capacity * loadFactor then resize
loadFactor :: Double
loadFactor = 0.75

-- | Type alias for a list of Key-Value Pairs
type Bucket k v = [(k, v)]

-- | Type alias for an array of lists of Key-ValuePair
-- The hash of the key will be the index of this array 
-- where the element may be stored
type BucketsArray stm k v = TArray stm Int (Bucket k v)

-- | Concurrent Hash Table data type
data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }

-- | Creates an empty Hash Table
newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = atomically $ do
  arr <- newArray (0, initCapacity - 1) [] >>= newTVar
  sz <- newTVar 0
  return $ CHT arr sz

-- | Gets Value from Key in CHT. If the key isn't found, returns `Nothing`
getCHT
  :: ( MonadConc m
     , Key k
     )
  => k
  -> CHT (STM m) k v
  -> m (Maybe v)
getCHT el cht = atomically $ do
  buckets <- readTVar $ chtBuckets cht
  cpcty <- getNumElements buckets
  let ind = hash el `mod` cpcty
  bucket <- readArray buckets ind
  return $ lookup el bucket

-- | Puts Key Value pair into CHT
putCHT
  :: ( MonadConc m
     , Key k
     )
  => k
  -> v
  -> CHT (STM m) k v
  -> m ()
putCHT k v cht = atomically $ do
  buckets <- readTVar $ chtBuckets cht
  size <- readTVar $ chtSize cht
  cpcty <- getNumElements buckets
  let ind = hash k `mod` cpcty
  bucket <- readArray buckets ind
  newBucket <- case lookup k bucket of
                 Just _  -> return $ filter (\(key, _) -> key /= k) bucket ++ [(k, v)]
                 Nothing -> do writeTVar (chtSize cht) (size + 1)
                               return $ bucket ++ [(k, v)]
  writeArray buckets ind newBucket
  when (fromIntegral size + 1 >= loadFactor * fromIntegral cpcty) $ do
    newArr <- newArray (0, cpcty * 2 - 1) []
    writeTVar (chtBuckets cht) newArr
    bucks <- getElems buckets
    forM_ bucks (\buck -> 
      forM_ buck (\kvPair@(k1, _) -> do 
        let ind2 = hash k1 `mod` (cpcty * 2)
        bucketNew <- readArray newArr ind2
        writeArray newArr ind2 (bucketNew ++ [kvPair])))

-- | returns the amount of elements that are put into the CHT
sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT ht = do readTVarConc $ chtSize ht

