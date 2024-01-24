module HW5.Evaluator
( eval
) where

import HW5.Base
import HW5.Impl

import Data.Maybe 
import Data.Map as M
import Data.Sequence as S
import Data.Foldable as F

import Control.Monad.Except
import Control.Monad.Trans.Except

{- | Basic Logic of Evaluation of Expressions of the Hi Language.
 - Implementation details are abstracted away to Impl.hs 
-}

-- | The main point of entry for our evaluator, it pattern matches 
-- on the value of HiExpr and depending on if the expression is a 
-- value, action, index application (on strings, lists, bytestrings)
-- returns the corresponding value equating to it.
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT $ case expr of
  HiExprValue val      -> return val
  HiExprRun action     -> evaluateAction action
  HiExprDict dict      -> evaluateDictionary dict
  HiExprApply fun list -> do 
    hival <- eval' fun
    case hival of
      HiValueFunction fn  -> evaluateFn fn list
      HiValueDict dict    -> evaluateDict dict list
      HiValueString _     -> evaluateSets hival list
      HiValueList _       -> evaluateSets hival list
      HiValueBytes _      -> evaluateSets hival list
      _invalid            -> throwE HiErrorInvalidFunction

-- | Created just so we won't litter our code with ExceptT
eval' :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
eval' v = ExceptT $ eval v

--  | Takes the element at a certian index or elements from a slice
--  in the container of elements.
--  Currently supports strings, lists, byteStrings, if the index is
--  null then it will be interpreted as 0, if a slice argument is null
--  then it will be interpreted as 0 or length depending on whether it's
--  the first or second argument
evaluateSets :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evaluateSets value [expr] = do
  v <- eval' expr
  hiIndex v value
evaluateSets value [expr1, expr2] = do
  v1 <- eval' expr1
  v2 <- eval' expr2
  hiSlice value v1 v2
evaluateSets _ _ = throwE HiErrorArityMismatch
  
-- | Function Application, takes in a Function and applies it to it's
-- arguments
evaluateFn :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evaluateFn fun = 
  case getArity fun of
    Unary     -> evalUnaryOp fun 
    Binary    -> evalBinOp fun 
    Trinary   -> evalTrinarOp fun 
    Arbitrary -> evalListOp fun

-- | Takes in a list of tuple expressions and creates a HiValueDict from them
-- the keys and values in the dictionary are evaluated and are HiValues
evaluateDictionary :: HiMonad m => [(HiExpr, HiExpr)] -> ExceptT HiError m HiValue
evaluateDictionary dict = do 
  list <- traverse mapExpr dict
  return $ HiValueDict $ M.fromList list
  where
    mapExpr (a, b) = do
      a' <- eval' a
      b' <- eval' b
      return (a', b')

-- | Searches whether the dictionary contains a value, if it doesn't returns
-- HiValueNull
evaluateDict :: HiMonad m => Map HiValue HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evaluateDict dict [expr] = do
  v <- eval' expr
  return $ fromMaybe HiValueNull (M.lookup v dict)
evaluateDict _ _ = throwE HiErrorArityMismatch

-- | Evaluates an action Expression
evaluateAction :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evaluateAction expr = do
  e <- eval' expr
  case e of
    HiValueAction action -> ExceptT $ Right <$> runAction action
    _invalid             -> throwE HiErrorInvalidArgument

-- | Evaluates Unary Expressions, is called from evaluateFn by assesing
-- whether the function is Unary, Binary or Trinary
evalUnaryOp :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalUnaryOp op [expr] = do
  val1 <- eval' expr 
  evalUnaryHiValue op val1
evalUnaryOp _ _ = throwE HiErrorArityMismatch

-- | Evaluates the expression by calling the implementation of the
-- called unary function
evalUnaryHiValue :: HiMonad m => HiFun -> HiValue -> ExceptT HiError m HiValue
evalUnaryHiValue HiFunNot         = hiNot 
evalUnaryHiValue HiFunLength      = hiLength 
evalUnaryHiValue HiFunReverse     = hiReverse
evalUnaryHiValue HiFunPackBytes   = hiPackBytes
evalUnaryHiValue HiFunUnpackBytes = hiUnpackBytes
evalUnaryHiValue HiFunEncodeUtf8  = hiEncodeUtf8
evalUnaryHiValue HiFunDecodeUtf8  = hiDecodeUtf8
evalUnaryHiValue HiFunZip         = hiZip
evalUnaryHiValue HiFunUnzip       = hiUnzip 
evalUnaryHiValue HiFunSerialise   = hiSerialise
evalUnaryHiValue HiFunDeserialise = hiDeserialise
evalUnaryHiValue HiFunRead        = hiRead
evalUnaryHiValue HiFunMkDir       = hiMkdir
evalUnaryHiValue HiFunChDir       = hiCd
evalUnaryHiValue HiFunParseTime   = hiTimeValue
evalUnaryHiValue HiFunEcho        = hiEcho
evalUnaryHiValue HiFunKeys        = hiKeys
evalUnaryHiValue HiFunValues      = hiValues
evalUnaryHiValue HiFunCount       = hiCount
evalUnaryHiValue HiFunInvert      = hiInvert
evalUnaryHiValue HiFunToLower     = hiToLower
evalUnaryHiValue HiFunToUpper     = hiToUpper 
evalUnaryHiValue HiFunTrim        = hiTrim 
evalUnaryHiValue _invalid         = const $ throwE HiErrorInvalidArgument

-- | Evaluates Binary Expressions, is called from evaluateFn by assesing
-- whether the function is Unary, Binary or Trinary. Also supports lazy
-- evaluation of the HiFunOr function and HiFunAnd function
evalBinOp :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalBinOp HiFunOr [lhs, rhs] = do
  val1 <- eval' lhs
  case val1 of
    HiValueBool False -> eval' rhs
    HiValueNull       -> eval' rhs
    _otherwise        -> return val1
evalBinOp HiFunAnd [lhs, rhs] = do
  val1 <- eval' lhs
  case val1 of
    HiValueBool False -> return val1
    HiValueNull       -> return val1
    _otherwise        -> eval' rhs
evalBinOp op [lhs, rhs] = do 
  val1 <- eval' lhs
  val2 <- eval' rhs
  matchBinary op val1 val2
evalBinOp _ _ = throwE HiErrorArityMismatch

-- | Evaluates the expression by calling the implementation of the
-- called binary function
matchBinary :: HiMonad m => HiFun -> HiValue -> HiValue -> ExceptT HiError m HiValue
matchBinary HiFunAdd            = hiAdd
matchBinary HiFunMul            = hiMul
matchBinary HiFunSub            = hiSub 
matchBinary HiFunDiv            = hiDiv 
matchBinary HiFunLessThan       = hiLT
matchBinary HiFunGreaterThan    = hiGT
matchBinary HiFunNotLessThan    = hiNLT
matchBinary HiFunNotGreaterThan = hiNGT
matchBinary HiFunEquals         = hiEQ
matchBinary HiFunNotEquals      = hiNEQ
matchBinary HiFunRange          = hiRange 
matchBinary HiFunWrite          = hiWrite 
matchBinary HiFunRand           = hiRandom 
matchBinary HiFunFold           = hiFold 
matchBinary _invalid            = \_ _ -> throwE HiErrorInvalidArgument

-- | implementation of HiFunFold, if the list is empty returns null
--
-- __Examples__:
--
-- >>> fold(add, [1, 2, 3, 4])
-- 10
--
-- >>> fold(div, ["some", "arbitrary", "file", "path"])
-- "some/arbitrary/file/path"
--
-- >>> fold(add, ["hello, ", "world"])
-- "hello world"
hiFold :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
hiFold (HiValueFunction _) (HiValueList Empty) = return HiValueNull
hiFold (HiValueFunction f) (HiValueList list)  = foldl1M (matchBinary f) (F.toList list)
hiFold _ _                                     = throwE HiErrorInvalidArgument

-- | Evaluates Trinary Expressions, is called from evaluateFn by assesing
-- whether the function is Unary, Binary or Trinary. Supports lazy evaluation
-- of the HiFunIf function
evalTrinarOp :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalTrinarOp HiFunIf [bool, lhs, rhs] = do
  val1 <- eval' bool
  case val1 of
    HiValueBool p -> if p 
                     then eval' lhs
                     else eval' rhs
    _notBoolType  -> throwE HiErrorInvalidArgument
evalTrinarOp _ _ = throwE HiErrorArityMismatch

-- | Takes in a list of expressions and creates a HiValueList from them
-- the elements in the list are evaluated into HiValues
evalListOp :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalListOp HiFunList list = do
  list' <- mapM eval' list
  return $ HiValueList $ S.fromList list'
evalListOp _ _            = throwE HiErrorInvalidFunction

