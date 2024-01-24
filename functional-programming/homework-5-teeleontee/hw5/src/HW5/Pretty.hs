module HW5.Pretty
  ( prettyValue
  ) where

import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle)

import Data.Ratio
import Data.Scientific
import Data.Word (Word8)
import Data.Time (UTCTime)
import Data.Foldable
import qualified Data.ByteString as B
import qualified Data.Map as M

import Numeric

import HW5.Base

-- | Prettyprints a HiValue
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber num)    = numberPrinter num
prettyValue (HiValueBool bool)     = boolPrinter bool
prettyValue (HiValueFunction func) = functionPrinter func
prettyValue HiValueNull            = pretty "null"
prettyValue (HiValueString text)   = pretty "\"" <> pretty text <> pretty "\""
prettyValue (HiValueList arr)      = listPrinter $ toList arr
prettyValue (HiValueBytes bytes)   = byteListPrinter $ B.unpack bytes
prettyValue (HiValueAction action) = actionPrinter action
prettyValue (HiValueTime time)     = timePrinter time
prettyValue (HiValueDict dict)     = dictPrinter dict

-- | Prettyprint implementation for HiValueDict
dictPrinter :: M.Map HiValue HiValue -> Doc AnsiStyle
dictPrinter d = encloseSep (pretty "{ ") (pretty " }") (pretty ", ") (map mapDict (M.toList d))
                  where
                    mapDict (a, b) = prettyValue a <> pretty ":" <+> prettyValue b

-- | Prettyprint implementation for HiFunParseTime
timePrinter :: UTCTime -> Doc AnsiStyle
timePrinter time = pretty "parse-time(\"" <> pretty (show time) <> pretty "\")"

-- | Prettyprint implementation for Bytestring
byteListPrinter :: [Word8] -> Doc AnsiStyle
byteListPrinter arr = encloseSep (pretty "[# ") (pretty " #]") (pretty " ") (map (pretty . toChar) arr)

-- | prettyprint a Word8 in hexadecimal format
toChar :: Word8 -> String
toChar a | a <= 15 = reverse $ showHex a "0"
         | otherwise = showHex a ""

-- | Prettyprints a list
listPrinter :: [HiValue] -> Doc AnsiStyle
listPrinter arr = encloseSep (pretty "[ ") (pretty " ]") (pretty ", ") (map prettyValue arr)

-- | Prettyprints a boolean
boolPrinter :: Bool -> Doc AnsiStyle
boolPrinter True  = pretty "true"
boolPrinter False = pretty "false"

-- | Prettyprints a HiAction
actionPrinter :: HiAction -> Doc AnsiStyle
actionPrinter action =
  case action of
    HiActionRead file      -> pretty "read(\"" <> pretty file <> pretty "\")"
    HiActionMkDir dir      -> pretty "mkdir(\"" <> pretty dir <> pretty "\")"
    HiActionChDir dir      -> pretty "cd(\"" <> pretty dir <> pretty "\")" 
    HiActionCwd            -> pretty "cwd"
    HiActionNow            -> pretty "now"
    HiActionRand l r       -> pretty "rand(" <+> pretty l <> pretty ", " <> pretty r <+> pretty ")"
    HiActionEcho text      -> pretty "echo(\"" <> pretty text <> pretty "\")"
    HiActionWrite file txt -> pretty "write(\"" <> pretty file <> pretty "\", \"" 
                              <> prettyValue (HiValueBytes txt) <> pretty "\")"

-- | Prettyprints a function
functionPrinter :: HiFun -> Doc AnsiStyle
functionPrinter fun = 
  pretty $ case fun of
    HiFunDiv            -> "div"
    HiFunMul            -> "mul"
    HiFunAdd            -> "add"
    HiFunSub            -> "sub"
    HiFunNot            -> "not"
    HiFunAnd            -> "and"
    HiFunOr             -> "or"
    HiFunLessThan       -> "less-than"
    HiFunGreaterThan    -> "greater-than"
    HiFunEquals         -> "equals"
    HiFunNotLessThan    -> "not-less-than"
    HiFunNotGreaterThan -> "not-greater-than"
    HiFunNotEquals      -> "not-equals"
    HiFunIf             -> "if"
    HiFunLength         -> "length"
    HiFunToUpper        -> "to-upper"
    HiFunToLower        -> "to-lower"
    HiFunReverse        -> "reverse"
    HiFunTrim           -> "trim"
    HiFunList           -> "list"
    HiFunRange          -> "range"
    HiFunFold           -> "fold"
    HiFunPackBytes      -> "pack-bytes"
    HiFunUnpackBytes    -> "unpack-bytes"
    HiFunEncodeUtf8     -> "encode-utf8"
    HiFunDecodeUtf8     -> "decode-utf8"
    HiFunZip            -> "zip"
    HiFunUnzip          -> "unzip"
    HiFunSerialise      -> "serialise"
    HiFunDeserialise    -> "deserialise"
    HiFunRead           -> "read"
    HiFunWrite          -> "write"
    HiFunMkDir          -> "mkdir"
    HiFunChDir          -> "cd"
    HiFunParseTime      -> "parse-time"
    HiFunRand           -> "rand"
    HiFunEcho           -> "echo"
    HiFunCount          -> "count"
    HiFunKeys           -> "keys"
    HiFunValues         -> "values"
    HiFunInvert         -> "invert"


-- | If a number is integral just prints it, but if not
-- calls the rationalNumberPrinter function
numberPrinter :: Rational -> Doc AnsiStyle
numberPrinter num = let a = numerator num
                        b = denominator num
                    in case b of
                         1 -> pretty a
                         _ -> rationalNumberPrinter num

-- | If a number is finite then calls finiteNumberPrinter else
-- calls infiniteNumberPrinter on the number
rationalNumberPrinter :: Rational -> Doc AnsiStyle
rationalNumberPrinter a = let (x, y) = fromRationalRepetendUnlimited a
                          in case y of
                               Nothing -> finiteNumberPrinter x
                               Just _  -> infiniteNumberPrinter a

-- | Prettyprints a scientific number
finiteNumberPrinter :: Scientific -> Doc AnsiStyle
finiteNumberPrinter a = pretty $ formatScientific Fixed Nothing a

-- | Breaks up a number into it's whoel part and remainder 
-- (so we can print a number like it's whole part plus a fraction)
infiniteNumberPrinter :: Rational -> Doc AnsiStyle
infiniteNumberPrinter a = let x = numerator a
                              y = denominator a
                              (w, q) = quotRem x y
                          in fractionalPrinter w q y

-- | If the whole part is zero just prettyprint the fraction
-- else prettyprint the whole part plus/minus the fraction
fractionalPrinter :: Integer -> Integer -> Integer -> Doc AnsiStyle 
fractionalPrinter whole remain denom 
  | whole == 0 = pretty remain <> slash <> pretty denom
  | otherwise  = if whole > 0
                 then pretty whole <+> pretty "+" <+> pretty remain <> slash <> pretty denom
                 else pretty whole <+> pretty "-" <+> pretty (remain * (-1)) <> slash <> pretty denom

