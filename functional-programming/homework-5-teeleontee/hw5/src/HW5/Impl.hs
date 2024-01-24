module HW5.Impl 
( hiLength
, hiCount
, hiAdd
, hiSub
, hiMul
, hiDiv
, hiLT
, hiGT
, hiNLT
, hiNGT
, hiEQ
, hiNEQ
, hiRange
, hiWrite
, hiNot
, hiReverse
, hiPackBytes
, hiUnpackBytes
, hiEncodeUtf8
, hiDecodeUtf8
, hiZip
, hiUnzip
, hiSerialise
, hiDeserialise
, hiRead
, hiMkdir
, hiCd
, hiEcho
, hiRandom
, hiTimeValue
, hiKeys
, hiValues
, hiInvert
, hiToLower
, hiToUpper
, hiTrim
, foldl1M
, hiSlice
, hiIndex
) where

import HW5.Base

import Data.Text as T
import Data.Map as M
import Data.Sequence as S
import Data.Foldable as F
import Data.Maybe
import Data.Time
import Data.Either
import Data.Semigroup (stimes)
import Data.Ratio 
import Data.Text.Encoding
import Data.Word

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

import Codec.Compression.Zlib
import Codec.Serialise

import Control.Monad.Except
import Control.Monad.Trans.Except

import Numeric (showHex, readHex)

import Text.Read (readMaybe)

-- | Returns the length of a list or a string
hiLength :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiLength value = do
  v <- getLength value
  return $ HiValueNumber $ toRational v

-- | Implementation for the count function, calls countString
-- or countList or countByteString depending on the argument
hiCount :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiCount value = case value of
  HiValueString s -> countString s
  HiValueList arr -> countList arr
  HiValueBytes bs -> countByteString bs
  _otherValue     -> throwE HiErrorInvalidArgument

-- | HiFunAdd implementation
hiAdd :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
hiAdd (HiValueBytes b1) (HiValueBytes b2)   = return $ HiValueBytes $ concatBytes b1 b2
hiAdd (HiValueString s1) (HiValueString s2) = return $ HiValueString $ T.append s1 s2
hiAdd (HiValueTime t) (HiValueNumber n)     = return $ HiValueTime $ addUTCTime (fromRational n) t
hiAdd (HiValueNumber n1) (HiValueNumber n2) = return $ HiValueNumber $ n1 + n2
hiAdd (HiValueList l1) (HiValueList l2)     = return $ HiValueList $ l1 >< l2
hiAdd _ _                                   = throwE HiErrorInvalidArgument

-- | HiFunMul implementation
hiMul :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
hiMul (HiValueNumber n1) (HiValueNumber n2) = return $ HiValueNumber $ n1 * n2
hiMul (HiValueNumber n) value               = hiStimes value n
hiMul value (HiValueNumber n)               = hiStimes value n
hiMul _ _                                   = throwE HiErrorInvalidArgument

-- | HiFunSub implementation
hiSub :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
hiSub (HiValueNumber n1) (HiValueNumber n2) = return $ HiValueNumber $ n1 - n2
hiSub (HiValueTime t1) (HiValueTime t2)     = return $ HiValueNumber $ toRational $ diffUTCTime t1 t2
hiSub _ _                                   = throwE HiErrorInvalidArgument

-- | HiFunDiv implementation
hiDiv :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
hiDiv (HiValueNumber _) (HiValueNumber 0)   = throwE HiErrorDivideByZero
hiDiv (HiValueNumber n1) (HiValueNumber n2) = return $ HiValueNumber $ n1 / n2
hiDiv (HiValueString s1) (HiValueString s2) = return $ HiValueString $ s1 `appendPath` s2
hiDiv _ _                                   = throwE HiErrorInvalidArgument

-- | HiFunLessThan implementation
hiLT :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
hiLT v1 v2 = return $ HiValueBool $ v1 < v2

-- | HiFunNotLessThan implementation
hiNLT :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
hiNLT v1 v2 = return $ HiValueBool $ v1 >= v2

-- | HiFunGreaterThan implementation
hiGT :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
hiGT v1 v2 = return $ HiValueBool $ v1 > v2

-- | HiFunNotGreaterThan implementation
hiNGT :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
hiNGT v1 v2 = return $ HiValueBool $ v1 <= v2

-- | HiFunEquals implementation
hiEQ :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
hiEQ value1 value2 = return $ HiValueBool $ value1 == value2

-- | HiFunNotEquals implementation
hiNEQ :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
hiNEQ value1 value2 = return $ HiValueBool $ value1 /= value2

-- | Monadic analog to foldl1 (like foldlM is to foldl) in the context of the Hi language
foldl1M :: (Foldable t, HiMonad m) => (a -> a -> ExceptT HiError m a) -> t a -> ExceptT HiError m a
foldl1M f list = case F.toList list of
                   []     -> throwE HiErrorInvalidArgument
                   (x:xs) -> foldM f x xs

-- | Range implementation
hiRange :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
hiRange (HiValueNumber v1) (HiValueNumber v2) = return $ HiValueList $ S.fromList $ fmap HiValueNumber [v1..v2]
hiRange _ _                                   = throwE HiErrorInvalidArgument

-- | implementation of string, list, bytes multiplication with a number
-- if the number isn't Natural throws HiErrorInvalidArgument.
--
-- >>> "hey" * 2
-- "heyhey"
hiStimes :: HiMonad m => HiValue -> Rational -> ExceptT HiError m HiValue
hiStimes val num | not $ isIntegral num || num < 0 = throwE HiErrorInvalidArgument
                 | otherwise = case val of
                                 HiValueString s -> return $ HiValueString $ stimes (numerator num) s
                                 HiValueList arr -> return $ HiValueList $ stimes (numerator num) arr
                                 HiValueBytes bs -> return $ HiValueBytes $ stimes (numerator num) bs
                                 _otherwise      -> throwE HiErrorInvalidArgument

-- HiFunWrite implementation
hiWrite :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
hiWrite (HiValueString s1) (HiValueString s2) = return $ HiValueAction $ HiActionWrite (T.unpack s1) (encodeUtf8 s2)
hiWrite _ _                                   = throwE HiErrorInvalidArgument

-- HiFunNot implementation
hiNot :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiNot (HiValueBool b) = return $ HiValueBool $ not b
hiNot _               = throwE HiErrorInvalidArgument

-- HiFunReverse implementation
hiReverse :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiReverse (HiValueList list) = return $ HiValueList $ S.reverse list
hiReverse (HiValueString s)  = return $ HiValueString $ T.reverse s
hiReverse _                  = throwE HiErrorInvalidArgument

-- HiFunPackBytes implementation
hiPackBytes :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiPackBytes (HiValueList list) = packBytes $ F.toList list
hiPackBytes _                  = throwE HiErrorInvalidArgument

-- HiFunUnpackBytes implementation
hiUnpackBytes :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiUnpackBytes (HiValueBytes bytes) = unpackBytes $ B.unpack bytes
hiUnpackBytes _                    = throwE HiErrorInvalidArgument

-- HiFunEncodeUtf8 implementation
hiEncodeUtf8 :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiEncodeUtf8 (HiValueString text) = return $ HiValueBytes $ encodeUtf8 text
hiEncodeUtf8 _                    = throwE HiErrorInvalidArgument

-- HiFunDecodeUtf8 implementation
hiDecodeUtf8 :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiDecodeUtf8 (HiValueBytes bytes) = return $ fromRight HiValueNull (HiValueString <$> decodeUtf8' bytes)
hiDecodeUtf8 _                    = throwE HiErrorInvalidArgument

-- HiFunZip implementation
hiZip :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiZip (HiValueBytes bytes) = return $ HiValueBytes $ BL.toStrict $ 
  compressWith defaultCompressParams { compressLevel = bestCompression } (BL.fromStrict bytes)
hiZip _ = throwE HiErrorInvalidArgument

-- HiFunUnzip implementation
hiUnzip :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiUnzip (HiValueBytes bytes) = return $ HiValueBytes $ BL.toStrict $ 
  decompressWith defaultDecompressParams (BL.fromStrict bytes)
hiUnzip _ = throwE HiErrorInvalidArgument

-- HiFunSerialise implementation
hiSerialise :: HiMonad m => Serialise a => a -> ExceptT HiError m HiValue
hiSerialise res = return $ HiValueBytes $ BL.toStrict $ serialise res

-- HiFunDeserialise implementation
hiDeserialise :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiDeserialise (HiValueBytes bytes) = case deserialiseOrFail $ BL.fromStrict bytes of
  Right res          -> return res
  Left _deserialFail -> throwE HiErrorInvalidArgument
hiDeserialise _ = throwE HiErrorInvalidArgument

-- HiFunRead implementation
hiRead :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiRead (HiValueString filePath) = return $ HiValueAction $ HiActionRead  $ T.unpack filePath
hiRead _                        = throwE HiErrorInvalidArgument
                        
-- HiFunMkDir implementation
hiMkdir :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiMkdir (HiValueString dir) = return $ HiValueAction $ HiActionMkDir $ T.unpack dir
hiMkdir _                   = throwE HiErrorInvalidArgument

-- HiFunChDir implementation
hiCd :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiCd (HiValueString dir) = return $ HiValueAction $ HiActionChDir $ T.unpack dir
hiCd _                   = throwE HiErrorInvalidArgument

-- HiFunParseTime implementation
hiTimeValue :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiTimeValue (HiValueString str) 
  = return $ maybe HiValueNull HiValueTime (readMaybe (T.unpack str) :: Maybe UTCTime)
hiTimeValue _ = throwE HiErrorInvalidArgument

-- HiFunEcho implementation
hiEcho :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiEcho (HiValueString str) = return $ HiValueAction $ HiActionEcho str
hiEcho _                   = throwE HiErrorInvalidArgument

-- HiFunKeys implementation
hiKeys :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiKeys (HiValueDict dict) = return $ HiValueList $ S.fromList $ M.keys dict
hiKeys _                  = throwE HiErrorInvalidArgument

-- HiFunValues implementation
hiValues :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiValues (HiValueDict dict) = return $ HiValueList $ S.fromList $ M.elems dict 
hiValues _                  = throwE HiErrorInvalidArgument

-- HiFunInvert implementation
hiInvert :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiInvert (HiValueDict d) = return $ HiValueDict $ M.map (HiValueList . S.fromList) (fromListWith (++) pairs)
  where
    pairs = [(v, [k]) | (k, v) <- M.toList d]
hiInvert _               = throwE HiErrorInvalidArgument

-- HiFunToLower implementation
hiToLower :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiToLower (HiValueString s) = return $ HiValueString $ T.toLower s
hiToLower _                 = throwE HiErrorInvalidArgument

-- HiFunToUpper implementation
hiToUpper :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiToUpper (HiValueString s) = return $ HiValueString $ T.toUpper s
hiToUpper _                 = throwE HiErrorInvalidArgument

-- HiFunTrim implementation
hiTrim :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiTrim (HiValueString s) = return $ HiValueString $ T.strip s
hiTrim _                 = throwE HiErrorInvalidArgument

-- HiFunRand implementation
hiRandom :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
hiRandom (HiValueNumber l) (HiValueNumber r) | isIntegral l && isIntegral r = 
                                                   let conv = fromIntegral . numerator 
                                                   in return $ HiValueAction $ HiActionRand 
                                                      (conv l) (conv r)
                                             | otherwise = throwE HiErrorInvalidArgument
hiRandom _ _ = throwE HiErrorInvalidArgument


-- | Implementation of slicing on supported containers
hiSlice :: HiMonad m => HiValue -> HiValue -> HiValue -> ExceptT HiError m HiValue
hiSlice value (HiValueNumber l) (HiValueNumber r) 
  = if not (isIntegral l) || not (isIntegral r)
    then throwE HiErrorInvalidArgument
    else do
      len <- getLength value
      let l' = slicePosInd len ((fromIntegral . numerator) l)
      let r' = slicePosInd len ((fromIntegral . numerator) r)
      case value of
        HiValueList arr -> return $ HiValueList $ S.drop l' $ S.take r' arr
        HiValueString s -> return $ HiValueString $ T.drop l' $ T.take r' s
        HiValueBytes bs -> return $ HiValueBytes $ B.drop l' $ B.take r' bs
        _otherwise      -> throwE HiErrorInvalidArgument
hiSlice value HiValueNull (HiValueNumber r) = hiSlice value (HiValueNumber 0) (HiValueNumber r)
hiSlice value (HiValueNumber l) HiValueNull = do
  len <- getLength value
  hiSlice value (HiValueNumber l) (HiValueNumber $ toRational len)
hiSlice _ _ _ = throwE HiErrorInvalidArgument

-- | Indexing implementation on supported containers
hiIndex :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
hiIndex HiValueNull value  = hiIndex (HiValueNumber 0) value
hiIndex (HiValueNumber num) value 
  | not $ isIntegral num = throwE HiErrorInvalidArgument
  | otherwise            = let ind = fromIntegral $ numerator num
                           in do 
                                len <- getLength value
                                if ind < 0 || ind >= len 
                                then return HiValueNull
                                else case value of
                                       HiValueString s -> return $ HiValueString $ pack [T.index s ind]
                                       HiValueBytes bs -> return $ HiValueNumber $ fromIntegral $ B.index bs ind
                                       HiValueList arr -> return $ fromMaybe HiValueNull (S.lookup ind arr)
                                       _otherwise      -> throwE HiErrorInvalidArgument
hiIndex _ _ = throwE HiErrorInvalidArgument

{- Helper functions for implementation -}

-- | Helper for implentation of pythonic slicing
slicePosInd :: BinOp Int
slicePosInd len a | a < 0     = a + len
                  | otherwise = a


-- | Length of container but returns int
getLength :: HiMonad m => HiValue -> ExceptT HiError m Int
getLength (HiValueBytes bytes) = return $ B.length bytes
getLength (HiValueString str)  = return $ T.length str
getLength (HiValueList list)   = return $ S.length list
getLength _invalid             = throwE HiErrorInvalidArgument

-- | Implementation of String division
appendPath :: BinOp Text
appendPath t1 t2 = t1 <> pack "/" <> t2

-- | Checks whether a Rational number is integral
isIntegral :: Rational -> Bool
isIntegral num = case denominator num of
  1 -> True
  _ -> False

-- | Basically casts a hex number to Word8 
toWordHex :: Monad m => String -> ExceptT HiError m Word8
toWordHex str = case readHex str of
                  [(n, "")] -> return $ fromInteger n
                  _invalid  -> throwE HiErrorInvalidArgument

-- | HiFunPackBytes detail Implementation 
packBytes :: Monad m => [HiValue] -> ExceptT HiError m HiValue
packBytes arr = do
  res <- traverse mapToHiNum arr
  return $ HiValueBytes $ B.pack res
  where
    mapToHiNum (HiValueNumber num) = toWordHex $ showHex (numerator num) ""
    mapToHiNum _invalid            = throwE HiErrorInvalidArgument

-- | HiFunUnpackBytes detail implementation
unpackBytes :: Monad m => [Word8] -> ExceptT HiError m HiValue
unpackBytes arr = let arr' = fmap (HiValueNumber . toRational) arr
                  in return $ HiValueList $ S.fromList arr'

-- | Concatinates ByteStrings together
concatBytes :: BinOp B.ByteString
concatBytes = B.append

-- | HiFunCount implementation for ByteStrings
countByteString :: HiMonad m => B.ByteString -> ExceptT HiError m HiValue
countByteString b = return $ HiValueDict $ 
  mapKeys (HiValueNumber . toRational) (countEls $ B.unpack b)

-- | HiFunCount implementation for Strings 
countString :: HiMonad m => Text -> ExceptT HiError m HiValue
countString t = return $ HiValueDict $ 
  mapKeys (HiValueString . T.singleton) (countEls $ T.unpack t)

-- | HiFunCount implementation for lists
countList :: HiMonad m => Seq HiValue -> ExceptT HiError m HiValue
countList s = return $ HiValueDict $ countEls $ F.toList s

-- | Helper function for HiFunCount that maps an element to its occurance
countEls :: Ord a => [a] -> Map a HiValue
countEls s = M.map HiValueNumber (fromListWith (+) freq)
  where
    freq = [(c, 1) | c <- s]

