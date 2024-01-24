module HW5.Parser
  ( parse
  ) where

import Data.Void (Void)
import Data.Word
import Data.Char
import Data.List (intercalate)
import Data.Sequence
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.ByteString as B

import Numeric (readHex)
import Control.Monad (void)
import qualified Control.Applicative as A

import Text.Megaparsec.Char 
import Text.Megaparsec as P hiding (parse)
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators.Expr

import HW5.Base (HiExpr (..), HiFun (..), HiValue (..), HiAction(..), BinOp)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space
  space1
  P.empty
  P.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

squareParens :: Parser a -> Parser a
squareParens = between (symbol "[") (symbol "]")

curlyParens :: Parser a -> Parser a
curlyParens = between (symbol "{") (symbol "}")

hexParens ::  Parser a -> Parser a
hexParens = between (string "[#" <* space)  (space *> string "#]")

-- | Parses the Grammar of the Hi language that is defined in Base.hs
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (space *> pHiExpression <* eof) "error"

pHiExpression :: Parser HiExpr
pHiExpression = do
  expr <- pHiExpression'
  dots <- optional $ pHiExprDot expr
  case dots of
    Nothing  -> return expr
    Just res -> return res

pHiExpression' :: Parser HiExpr
pHiExpression' = makeExprParser pTerm operatorTable

pTerm :: Parser HiExpr
pTerm = lexeme $ choice 
  [ parens pHiExpression'
  , pHiExpr
  ]

operatorTable :: [[Operator Parser HiExpr]]
operatorTable = 
  [ [ binaryL "*"                $ wrapFunc HiFunMul
    , binaryNotFolledByL "/" "=" $ wrapFunc HiFunDiv
    ]
  , [ binaryL "+" $ wrapFunc HiFunAdd
    , binaryL "-" $ wrapFunc HiFunSub
    ]
  , [ binaryN "<=" $ wrapFunc HiFunNotGreaterThan
    , binaryN "<"  $ wrapFunc HiFunLessThan
    , binaryN ">=" $ wrapFunc HiFunNotLessThan
    , binaryN ">"  $ wrapFunc HiFunGreaterThan
    , binaryN "==" $ wrapFunc HiFunEquals
    , binaryN "/=" $ wrapFunc HiFunNotEquals
    ]
  , [ binaryR "&&" $ wrapFunc HiFunAnd ]
  , [ binaryR "||" $ wrapFunc HiFunOr ]
  ]

wrapFunc :: HiFun -> HiExpr -> HiExpr -> HiExpr
wrapFunc f a b = HiExprApply (HiExprValue $ HiValueFunction f) [a, b]

binaryL :: String -> BinOp HiExpr -> Operator Parser HiExpr
binaryL funName fun = InfixL $ fun <$ symbol funName

binaryN :: String -> BinOp HiExpr -> Operator Parser HiExpr
binaryN funName fun = InfixN $ fun <$ symbol funName

binaryR :: String -> BinOp HiExpr -> Operator Parser HiExpr
binaryR funName fun = InfixR $ fun <$ symbol funName

binaryNotFolledByL :: String -> String -> BinOp HiExpr -> Operator Parser HiExpr
binaryNotFolledByL funName banned fun 
  = InfixL $ fun <$ try (symbol funName <* notFollowedBy (string banned))

pHiFunction :: Parser HiFun
pHiFunction = choice
  [ HiFunDiv            <$ string "div" 
  , HiFunAdd            <$ string "add"
  , HiFunSub            <$ string "sub"
  , HiFunMul            <$ string "mul"
  , HiFunAnd            <$ string "and"
  , HiFunOr             <$ string "or"
  , HiFunLessThan       <$ string "less-than"
  , HiFunGreaterThan    <$ string "greater-than"
  , HiFunEquals         <$ string "equals"
  , HiFunNotLessThan    <$ string "not-less-than"
  , HiFunNotGreaterThan <$ string "not-greater-than"
  , HiFunNotEquals      <$ string "not-equals"
  , HiFunNot            <$ string "not"
  , HiFunIf             <$ string "if"
  , HiFunLength         <$ string "length"
  , HiFunToUpper        <$ string "to-upper"
  , HiFunToLower        <$ string "to-lower"
  , HiFunReverse        <$ string "reverse"
  , HiFunTrim           <$ string "trim"
  , HiFunList           <$ string "list"
  , HiFunRange          <$ string "range"
  , HiFunFold           <$ string "fold"
  , HiFunPackBytes      <$ string "pack-bytes"
  , HiFunUnpackBytes    <$ string "unpack-bytes"
  , HiFunZip            <$ string "zip"
  , HiFunUnzip          <$ string "unzip"
  , HiFunEncodeUtf8     <$ string "encode-utf8"
  , HiFunDecodeUtf8     <$ string "decode-utf8"
  , HiFunSerialise      <$ string "serialise"
  , HiFunDeserialise    <$ string "deserialise"
  , HiFunRead           <$ string "read"
  , HiFunWrite          <$ string "write"
  , HiFunMkDir          <$ string "mkdir"
  , HiFunChDir          <$ string "cd"
  , HiFunParseTime      <$ string "parse-time"
  , HiFunRand           <$ string "rand"
  , HiFunEcho           <$ string "echo"
  , HiFunCount          <$ string "count"
  , HiFunKeys           <$ string "keys"
  , HiFunValues         <$ string "values"
  , HiFunInvert         <$ string "invert"
  ]

pHiBool :: Parser HiValue
pHiBool = lexeme $ choice 
  [ HiValueBool True  <$ string "true"
  , HiValueBool False <$ string "false"
  ]

pHiBinaryFunc :: Parser HiValue
pHiBinaryFunc = lexeme $ HiValueFunction <$> pHiFunction

pHiValueAction :: Parser HiAction
pHiValueAction = choice 
  [ HiActionCwd <$ string "cwd"
  , HiActionNow <$ string "now"
  ]

pHiNumber :: Parser HiValue
pHiNumber = lexeme $ do
  res <- toRational <$> L.signed sc L.scientific
  return $ HiValueNumber res

pHiString :: Parser HiValue
pHiString = lexeme $ do
  str <- char '"' >> manyTill L.charLiteral (char '"')
  return $ HiValueString $ T.pack str

pHiNull :: Parser HiValue
pHiNull = do
  void $ symbol "null"
  return HiValueNull

pHiBytes :: Parser HiValue
pHiBytes = lexeme $ do
  list <- hexParens pHiBytesList
  return $ HiValueBytes $ B.pack list

pHiAction :: Parser HiValue
pHiAction = lexeme $ HiValueAction <$> pHiValueAction

pHiDict :: Parser HiValue
pHiDict = lexeme $ curlyParens $ HiValueDict . M.fromList <$> pHiListOfPairs

pHiValue :: Parser HiValue
pHiValue = 
      pHiNumber 
  <|> pHiBinaryFunc 
  <|> pHiBool 
  <|> pHiString 
  <|> pHiNull 
  <|> pHiBytes 
  <|> pHiAction 
  <|> pHiListOfValues 
  <|> pHiDict

pHiListOfExprPairs :: Parser [(HiExpr, HiExpr)]
pHiListOfExprPairs = sepBy (space *> pHiKeyValueExprs) (char ',')

pHiDictExpr :: Parser HiExpr
pHiDictExpr = lexeme $ do
  dict <- curlyParens pHiListOfExprPairs
  pHiExpr' $ HiExprDict dict

pHiKeyValueExprs :: Parser (HiExpr, HiExpr)
pHiKeyValueExprs = lexeme $ do
  key <- pHiExpression'
  void $ symbol ":"
  val <- pHiExpression'
  return (key, val)

pHiListOfPairs :: Parser [(HiValue, HiValue)]
pHiListOfPairs = sepBy (space *> pHiKeyValuePairs) (char ',')

pHiKeyValuePairs :: Parser (HiValue, HiValue)
pHiKeyValuePairs = lexeme $ do
  key   <- pHiValue 
  void $ symbol ":"
  value <- pHiValue
  return (key, value)

hexSingle :: Parser Word8
hexSingle = do
  v1 <- hexDigitChar
  v2 <- hexDigitChar
  let val = readHexDigit [v1, v2]
  case val of
    Just x  -> return x
    Nothing -> fail "couldn't parse hex"
  where
    readHexDigit :: String -> Maybe Word8
    readHexDigit hex = case readHex hex of
                         [(n, "")] -> Just n
                         _failed   -> Nothing

pHiExprList :: Parser [HiExpr]
pHiExprList = sepBy (space *> pHiExpression') (char ',')

pHiValueList :: Parser [HiValue]
pHiValueList = sepBy (space *> pHiValue) (char ',')

pHiBytesList :: Parser [Word8]
pHiBytesList = sepEndBy hexSingle (some $ char ' ')

pHiListOfExprs:: Parser HiExpr
pHiListOfExprs = do
  list <- squareParens pHiExprList
  return $ HiExprApply (HiExprValue (HiValueFunction HiFunList)) list

pHiListOfValues :: Parser HiValue
pHiListOfValues = lexeme $ do
  list <- squareParens pHiValueList
  return $ HiValueList $ fromList list

pHiListExpr :: Parser HiExpr
pHiListExpr = do
  list <- pHiListOfExprs
  pHiExpr' list

pHiValueExpr :: Parser HiExpr
pHiValueExpr = lexeme $ do
  value <- space *> (parens pHiValue <|> pHiValue)
  pHiExpr' $ HiExprValue value

pHiExpr :: Parser HiExpr
pHiExpr = try pHiListExpr 
  <|> try pHiDictExpr 
  <|> pHiValueExpr

pHiExprRecurse :: HiExpr -> Parser HiExpr
pHiExprRecurse value = pHiExprDot value <|> pHiExprArgs value

pHiExprArgs :: HiExpr -> Parser HiExpr
pHiExprArgs value = do
  exprs <- parens pHiExprList
  pHiExpr' $ HiExprApply value exprs

pHiExprDot :: HiExpr -> Parser HiExpr
pHiExprDot value = do
  void $ symbol "."
  l <- pHiKeyLiteral
  pHiExpr' $ HiExprApply value [HiExprValue l]

pHiKeyLiteral :: Parser HiValue 
pHiKeyLiteral =  lexeme $ do 
  chars <- ((:) <$> satisfy isAlpha <*> A.many (satisfy isAlphaNum)) `sepBy1` char '-'
  return $ HiValueString $ T.pack $ intercalate "-" chars

pHiExpr' :: HiExpr -> Parser HiExpr 
pHiExpr' v = pHiExprRecurse v <|> pHiExprAction v

pHiExprAction :: HiExpr -> Parser HiExpr
pHiExprAction value = do 
  s <- optional $ symbol "!"
  case s of
    Just _  -> return $ HiExprRun value
    Nothing -> return value
