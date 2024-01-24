module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import HW3.T1

newtype State s a = S { runS :: s -> Annotated s a } 

mapState :: (a -> b) -> State s a -> State s b
mapState fn s = S $ \i -> let a' :# e = runS s i
                          in fn a' :# e

wrapState :: a -> State s a
wrapState a = S $ \i -> a :# i

joinState :: State s (State s a) -> State s a
joinState s = S $ \x -> let (s' :# e) = runS s x
                        in runS s' e

modifyState :: (s -> s) -> State s ()
modifyState fn = S $ \x -> () :# fn x

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  fn <*> s1 = S $ \x -> let fn' :# e = runS fn x
                            s2 = fmap fn' s1
                        in runS s2 e

instance Monad (State s) where
  return = pure
  s >>= fn = S $ \x -> let s' :# e = runS s x
                       in runS (fn s') e

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  a + b = Op $ Add a b
  a - b = Op $ Sub a b
  a * b = Op $ Mul a b
  abs a = Op $ Abs a
  signum a = Op $ Sgn a
  fromInteger a = Val $ fromInteger a

instance Fractional Expr where
  a / b = Op $ Div a b
  fromRational a = Val $ fromRational a

type BinOp a = a -> a -> a

evalBinaryOp 
  :: Expr 
  -> Expr 
  -> (Double -> Double -> Prim Double)
  -> BinOp Double 
  -> State [Prim Double] Double
evalBinaryOp a b fn op = do
  lhs <- eval a
  rhs <- eval b
  modifyState $ \s -> fn lhs rhs : s
  return $ op lhs rhs

evalUnaryOp 
  :: Expr 
  -> (Double -> Prim Double) 
  -> (Double -> Double) 
  -> State [Prim Double] Double
evalUnaryOp a fn op = do
  expr <- eval a
  modifyState $ \s -> fn expr : s
  return $ op expr

eval :: Expr -> State [Prim Double] Double
eval (Op (Add a b)) = evalBinaryOp a b Add (+)
eval (Op (Sub a b)) = evalBinaryOp a b Sub (-)
eval (Op (Mul a b)) = evalBinaryOp a b Mul (*)
eval (Op (Div a b)) = evalBinaryOp a b Div (/)
eval (Op (Abs a))   = evalUnaryOp a Abs abs
eval (Op (Sgn a))   = evalUnaryOp a Sgn signum
eval (Val a)        = return a

