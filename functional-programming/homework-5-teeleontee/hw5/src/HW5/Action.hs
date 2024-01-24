{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
module HW5.Action (
  HIO (..)
, HiPermission (..) 
, PermissionException (..)
) where

import Prelude hiding (read)

import HW5.Base
import Data.Text.Encoding

import Data.Set
import Data.Text as T
import Data.Sequence as S
import Data.Time (getCurrentTime)
import Data.Either
import qualified Data.ByteString as B

import Control.Exception
import Control.Monad.Trans.Reader

import System.Directory
import System.Random

-- | Permissions for read/write/time operations
data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)

-- Exception class for when the user doesn't have permission
-- for an operation
newtype PermissionException =
  PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

-- | IO for the Hi language takes in a set of the users permissions
-- and returns an IO operation
newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a } 
  deriving (Functor, Applicative, Monad) via (ReaderT (Set HiPermission) IO)

-- | Wraps context for HIO and computes the operation
runAction' :: Maybe HiPermission -> IO HiValue -> HIO HiValue
runAction' perm func = HIO $ \s -> case perm of
                                     Nothing    -> func
                                     Just perm' -> if perm' `elem` s
                                                   then func
                                                   else throwIO $ PermissionRequired perm'

-- | HiMonad instances for each Action defined by the Hi language
instance HiMonad HIO where
  runAction :: HiAction -> HIO HiValue
  runAction HiActionNow              = runAction' (Just AllowTime) now
  runAction HiActionCwd              = runAction' (Just AllowRead) cwd
  runAction (HiActionMkDir fp)       = runAction' (Just AllowWrite) (mkdir fp)
  runAction (HiActionChDir fp)       = runAction' (Just AllowRead) (cd fp)
  runAction (HiActionRead fp)        = runAction' (Just AllowRead) (read fp)
  runAction (HiActionWrite fp bytes) = runAction' (Just AllowWrite) (write fp bytes)
  runAction (HiActionEcho text)      = runAction' (Just AllowWrite) (echo text)
  runAction (HiActionRand l r)       = runAction' Nothing $ rand l r

-- | Echo implementation just outputs the input to IO
echo :: Text -> IO HiValue
echo text = do
  putStrLn $ T.unpack text
  return HiValueNull

-- | Random number in range action implementation
rand :: Int -> Int -> IO HiValue
rand l r = do
  n <- getStdRandom $ uniformR (l, r)
  return $ HiValueNumber $ toRational n

-- | outputs the current working directory to IO
cwd :: IO HiValue
cwd = HiValueString . T.pack <$> getCurrentDirectory

-- | Creates a directory, name of the directory is the argument
mkdir :: FilePath -> IO HiValue
mkdir fp = do
  createDirectory fp
  return HiValueNull

-- | Changes directory to the one specified in the argument
cd :: FilePath -> IO HiValue
cd fp = do
  setCurrentDirectory fp
  return HiValueNull
   
-- | Reads data from a called filePath 
read :: FilePath -> IO HiValue
read fp = do
  isFile <- doesFileExist fp
  if isFile
  then decodeUtf8Bytes <$> B.readFile fp
  else do
    list <- listDirectory fp
    return $ HiValueList $ S.fromList $ fmap (HiValueString . T.pack) list
  where
    decodeUtf8Bytes bytes = fromRight HiValueNull (HiValueString <$> decodeUtf8' bytes)

-- | Writes data to a filepath
write :: FilePath -> B.ByteString -> IO HiValue
write fp bytes = do
  B.writeFile fp bytes
  return HiValueNull

-- | Returns current time
now :: IO HiValue
now = HiValueTime <$> getCurrentTime

