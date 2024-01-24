{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import Numeric.Natural (Natural)
import Control.Applicative
import Control.Monad

import HW4.Types
import HW4.T1 (ExceptState(..))
import Data.Char (isDigit, isSpace, digitToInt)

-- | throws in the Parser Monad 
newtype ParseError = ErrorAtPos Natural
  deriving (Show, Eq)

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

-- | Runs a computation of the parser
runP :: Parser a -> String -> Except ParseError a
runP (P st) s = let x = runES st (0, s)
                in case x of
                  Error e          -> Error e
                  Success (a :# _) -> Success a

instance Alternative Parser where
  empty = parseError
  P p1 <|> P p2 = P $ ES $ \pair -> 
                                let x = runES p1 pair
                                in case x of
                                     Error _   -> runES p2 pair
                                     success   -> success

instance MonadPlus Parser

{- | The point of entry for our
  parser of arithmetic expressions.

  We are parsing the following CFG 
  that is LL(1); The following are the production rules for the grammar

    * S  -> E

    * E  -> T E'

    * E' -> + T E'  |  - T E'  |  eps

    * T  -> F T'
    
    * T' -> * F T'  |  / F T'  |  eps
    
    * F  -> double  |  ( E )

  Examples of use are given below

  >>> parseExpr "3.14 + 1.618 * 2"
  Success (Op (Add (Val 3.14) (Op (Mul (Val 1.618) (Val 2.0)))))

  Can throw an Error if given unexpected input

  >>> parseExpr "24 + Hello"
  Error (ErrorAtPos 3)
-}
parseExpr :: String -> Except ParseError Expr
parseExpr = runP start

-- | The production rule S -> E
-- The following functions are not strictly 
-- following the CFG production as we do not
-- have a lexical analyzer. But still they mostly
-- represent each rule
start :: Parser Expr
start = pE <* pUntilEof

-- | The production rule E -> T E' 
-- if E' happens to be eps we return T
pE :: Parser Expr
pE = do 
  t <- pT
  pE' t <|> return t

binOp 
  :: (Expr -> Expr -> Prim Expr) 
  -> Expr 
  -> Expr 
  -> Parser Expr
binOp fn e1 e2 = return $ Op $ fn e1 e2

-- | The production rules E' -> + T E' | - T E'
-- Computes until E' unwraps into eps
pE' :: Expr -> Parser Expr
pE' tFromAbove = 
  do
    skipSpaces
    op  <- pChar
    guard (op == '+' || op == '-') -- to save pos
    t   <- pT
    res <- case op of
      '+' -> binOp Add tFromAbove t
      '-' -> binOp Sub tFromAbove t
      _   -> empty
    pE' res <|> return res 

-- | The production rule T -> F T'
-- if T' unwraps to eps only returns f
pT :: Parser Expr
pT = do
  f <- pF
  pT' f <|> return f

-- | The production rules T' -> * F T' | / F T'
-- Computes until T' unwraps into eps
--
pT' :: Expr -> Parser Expr
pT' fFromAbove = do 
  skipSpaces
  op <- pChar
  guard (op == '*' || op == '/') -- to save pos
  f  <- pF
  res <- case op of
           '*' -> binOp Mul fFromAbove f
           '/' -> binOp Div fFromAbove f
           _   -> empty
  pT' res <|> return res 

-- | The production rules F -> double | ( E )
pF :: Parser Expr
pF = skipSpaces >> 
  Val <$> pDouble -- <|> pDouble')
  <|> pEqChar '(' *> pE <* skipSpaces <* pEqChar ')'

---------- Parser Building Blocks ----------

-- | Throws in the parser monad
parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error $ ErrorAtPos pos

-- | Consumes a single char from the string
-- returns an Error if unable. 
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (x:xs) -> Success (x :# (pos + 1, xs))

pEof :: Parser ()
pEof = P $ ES $ \pair@(pos, s) ->
  case s of
    []     -> Success (() :# pair)
    (_:_)  -> Error (ErrorAtPos pos) 
    -- my lsp did not like _ ->

pUntilEof :: Parser ()
pUntilEof = skipSpaces >> pEof

pSat :: (Char -> Bool) -> Parser Char
pSat fn = mfilter fn pChar

pDigit :: Parser Char
pDigit = pSat isDigit

pEqChar :: Char -> Parser Char
pEqChar c = pSat (==c)

pNum :: Parser String
pNum = some pDigit

pSpace :: Parser Char
pSpace = pSat isSpace

skipSpaces :: Parser ()
skipSpaces = void $ many pSpace

pGenericNumWithLength :: Num a => (Char -> a) -> Parser (a, Int)
pGenericNumWithLength fn = do
  ints <- fmap (fmap fn) pNum
  return (foldInts ints, length ints)
    where
      foldInts = foldl1 $ (+) . (*10)

pInt :: Parser Integer
pInt = do 
  pint <- pGenericNumWithLength $ toInteger . digitToInt
  return $ fst pint

pDoubleWithLength :: Parser (Double, Int)
pDoubleWithLength = pGenericNumWithLength $ fromIntegral . digitToInt
 
pDouble :: Parser Double
pDouble = do
  before <- pDouble'
  do 
    void $ pEqChar '.'
    (after, cnt) <- pDoubleWithLength
    return $ let x = 10 ^ cnt
                 res = (toRational before * x + toRational after) / x
             in fromRational res 
   <|> return before

pDouble' :: Parser Double
pDouble' = fromInteger <$> pInt

