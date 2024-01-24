{-# LANGUAGE DeriveGeneric #-}
module HW5.Base
  ( HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiValue (..)
  , Arity (..)
  , HiMonad (..)
  , HiAction (..)
  , BinOp
  , getArity
  ) where

import Data.Map
import Data.Text 
import Data.Time
import Data.Sequence
import Data.ByteString 

import Codec.Serialise

import GHC.Generics

-- | Type alias for a closed binary operation
type BinOp a = a -> a -> a

-- | Function arity data class
data Arity
  = Unary
  | Binary
  | Trinary
  | Arbitrary

-- | Functions supported by the Hi language
data HiFun 
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunEquals
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf  
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Show, Eq, Generic, Ord)

-- | Values in the Hi language
data HiValue 
  = HiValueBool Bool
  | HiValueNumber Rational 
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Generic, Ord)

-- | Expressions in the Hi language
data HiExpr 
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show, Eq)

-- | Errors in the Hi language
data HiError 
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Generic)

-- | Actions in the Hi language
data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Show, Eq, Generic, Ord)

-- | HiMonad typeclass for computing HiActions
class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

instance Serialise HiValue
instance Serialise HiFun
instance Serialise HiError
instance Serialise HiAction

-- | Returns the arity of a HiFun 
getArity :: HiFun -> Arity
getArity f = 
  case f of
    HiFunLessThan       -> Binary
    HiFunAdd            -> Binary
    HiFunSub            -> Binary
    HiFunMul            -> Binary
    HiFunDiv            -> Binary
    HiFunNot            -> Unary
    HiFunAnd            -> Binary
    HiFunOr             -> Binary
    HiFunGreaterThan    -> Binary
    HiFunEquals         -> Binary
    HiFunNotLessThan    -> Binary
    HiFunNotGreaterThan -> Binary
    HiFunNotEquals      -> Binary
    HiFunIf             -> Trinary
    HiFunLength         -> Unary
    HiFunToUpper        -> Unary
    HiFunToLower        -> Unary
    HiFunReverse        -> Unary
    HiFunTrim           -> Unary
    HiFunList           -> Arbitrary
    HiFunRange          -> Binary
    HiFunFold           -> Binary
    HiFunPackBytes      -> Unary
    HiFunUnpackBytes    -> Unary
    HiFunEncodeUtf8     -> Unary
    HiFunDecodeUtf8     -> Unary
    HiFunZip            -> Unary
    HiFunUnzip          -> Unary
    HiFunSerialise      -> Unary
    HiFunDeserialise    -> Unary
    HiFunRead           -> Unary
    HiFunWrite          -> Binary
    HiFunMkDir          -> Unary
    HiFunChDir          -> Unary
    HiFunParseTime      -> Unary
    HiFunRand           -> Binary
    HiFunEcho           -> Unary
    HiFunCount          -> Unary
    HiFunKeys           -> Unary
    HiFunValues         -> Unary
    HiFunInvert         -> Unary

