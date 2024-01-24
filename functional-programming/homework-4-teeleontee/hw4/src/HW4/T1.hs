module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import HW4.Types
import Control.Monad

-- | A State Monad that supports error handling  
newtype ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

-- | basically `fmap` for `ExceptState`
mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState fn st = ES $ \s -> let x = runES st s
                                  in case x of
                                    Success (a :# e) -> Success $ fn a :# e
                                    Error err        -> Error err

-- | Wraps a in a `ExceptState` monad
wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success $ a :# s

-- | Joins `ExceptState`'s into one
joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState st = ES $ \s -> let x = runES st s
                                in case x of
                                  Error err -> Error err
                                  Success (a :# e) -> runES a e

-- | Modifies the State of `ExceptsState` by taking in a function `s -> s`
modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState fn = ES $ \s -> Success $ () :# fn s

-- | Throws Error no matter what
throwExceptState :: e -> ExceptState e s a
throwExceptState err = ES $ \_ -> Error err

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = Control.Monad.ap

instance Monad (ExceptState e s) where
  st >>= fn = joinExceptState $ fmap fn st

data EvaluationError = DivideByZero
  deriving (Show, Eq)

-- | Evaluates Expression and stores the state trace, if 
-- an expression contrains division by zero - throws error
eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Op (Add a b)) = evalBinaryOp a b Add (+)
eval (Op (Sub a b)) = evalBinaryOp a b Sub (-)
eval (Op (Mul a b)) = evalBinaryOp a b Mul (*)
eval (Op (Div a b)) = evalDivOp a b
eval (Op (Abs a))   = evalUnaryOp a Abs abs
eval (Op (Sgn a))   = evalUnaryOp a Sgn signum
eval (Val a)        = return a

type BinOp a = a -> a -> a

evalBinaryOp 
  :: Expr 
  -> Expr 
  -> (Double -> Double -> Prim Double)
  -> BinOp Double 
  -> ExceptState EvaluationError [Prim Double] Double
evalBinaryOp a b fn op = do
  lhs <- eval a
  rhs <- eval b
  modifyExceptState $ \s -> fn lhs rhs : s
  return $ op lhs rhs

evalUnaryOp 
  :: Expr 
  -> (Double -> Prim Double) 
  -> (Double -> Double) 
  -> ExceptState EvaluationError [Prim Double] Double
evalUnaryOp a fn op = do
  expr <- eval a
  modifyExceptState $ \s -> fn expr : s
  return $ op expr

evalDivOp
  :: Expr 
  -> Expr 
  -> ExceptState EvaluationError [Prim Double] Double
evalDivOp a b = do
  lhs <- eval a
  rhs <- eval b
  modifyExceptState $ \s -> Div lhs rhs : s
  guard' (rhs /= 0)
  return $ lhs / rhs
  where 
    guard' True = return ()
    guard' _    = throwExceptState DivideByZero

