import Test.Tasty
import Test.Tasty.HUnit

import HW4.T2
import HW4.T1
import HW4.Types

parserTests :: TestTree
parserTests = testGroup
  "Tests for Homework 4"
  [
    testCase "Checking Parse Tree - Basic Arithmetic" $
      do
        assertEqual "2 + 2 ="    (Success $ Op $ Add (Val 2.0) (Val 2.0))    (parseExpr "2 + 2")
        assertEqual "2 - 93 ="   (Success $ Op $ Sub (Val 2.0) (Val 93.0))   (parseExpr "2 - 93")
        assertEqual "212 * 93 =" (Success $ Op $ Mul (Val 212.0) (Val 93.0)) (parseExpr "212 * 93")
        assertEqual "90 * 78 ="  (Success $ Op $ Mul (Val 90.0) (Val 78.0))  (parseExpr "90 * 78"),

    testCase "Checking Parse Tree - Operation Precidence" $
      do 
        assertEqual "2 + 2 * 5 ="       (Success $ Op $ Add (Val 2.0) (Op $ Mul (Val 2.0) (Val 5.0))) 
                                        (parseExpr "2 + 2 * 5")
        assertEqual "2 + 2 * 5 + 3 ="   (Success (Op (Add (Op (Add (Val 2.0) (Op (Mul (Val 2.0) (Val 5.0))))) (Val 3.0)))) 
                                        (parseExpr "2 + 2 * 5 + 3")
        assertEqual "2 / 1 * 5 ="       (Success $ Op $ Mul (Op $ Div (Val 2.0) (Val 1.0)) (Val 10.0)) 
                                        (parseExpr "2 / 1 * 10")
        assertEqual "2 - 1 + 5 ="       (Success $ Op $ Add (Op $ Sub (Val 2.0) (Val 1.0)) (Val 5.0)) 
                                        (parseExpr "2 - 1 + 5")
        assertEqual "2 + 2 * (5 + 3) =" (Success $ Op $ Mul (Op $ Add (Val 2.0) (Val 2.0)) (Op $ Add (Val 5.0) (Val 3.0)))
                                        (parseExpr "(2 + 2) * (5 + 3)"),

    testCase "Checking Whitespaces Parsing" $
      do 
        assertEqual "   2         "   (Success $ Val 2.0) 
                                      (parseExpr "   2         ")
        assertEqual "2+2*5 ="         (Success $ Op $ Add (Val 2.0) (Op $ Mul (Val 2.0) (Val 5.0))) 
                                      (parseExpr "2+2*5")
        assertEqual "  2 +2 *5 +3  =" (Success (Op (Add (Op (Add (Val 2.0) (Op (Mul (Val 2.0) (Val 5.0))))) (Val 3.0))))
                                      (parseExpr "  2 +2 *5 +3  "),

    testCase "Parsing Numbers Smoke" $
      do
        assertEqual "123456789101112" (Success $ Val 123456789101112.0) (parseExpr "123456789101112.0")
        assertEqual "1234.0 + 1234"   (Success $ Op $ Add (Val 1234.0) (Val 1234.0)) (parseExpr "1234.0 + 1234")
        assertEqual "1234.000 + 1234"   (Success $ Op $ Add (Val 1234.0) (Val 1234.0)) (parseExpr "1234.0 + 1234"),

    testCase "Checking Easy Double Parsing" $
      do
       assertEqual "1234.00"        (Success $ Val 1234.0) (parseExpr "1234.00")
       assertEqual "12"             (Success $ Val 12.0) (parseExpr "12")
       assertEqual "123456789"      (Success $ Val 123456789.0) (parseExpr "123456789")
       assertEqual "1.5"            (Success $ Val 1.5) (parseExpr "1.5"),

    testCase "Checking Hard Double Parsing " $
      do
        assertEqual "1.125"                (Success $ Val 1.125) (parseExpr "1.125")
        assertEqual "1.123456789"          (Success $ Val 1.123456789) (parseExpr "1.123456789")
        assertEqual "3.14159265358979"     (Success $ Val 3.14159265358979) (parseExpr "3.14159265358979")
        assertEqual "2.71828182845904"     (Success $ Val 2.71828182845904) (parseExpr "2.71828182845904")
        assertEqual "2.71828182845904 + 1" (Success $ Op $ Add (Val 2.71828182845904) (Val 1.0)) 
                                           (parseExpr "2.71828182845904 + 1")
        assertEqual "3.14 + 1.618 * 2"     (Success (Op (Add (Val 3.14) (Op (Mul (Val 1.618) (Val 2.0)))))) 
                                           (parseExpr "3.14 + 1.618 * 2"),
    
    testCase "Checking Evaluation Functionality" $
      do
        assertEqual "2 + 2 ="            (runES (eval (2 + 2)) []) (Success (4 :# [Add 2 2])) 
        assertEqual "2 + 3 * 5 - 7"      (runES (eval (2 + 3 * 5 - 7)) []) (Success (10 :# [Sub 17 7, Add 2 15, Mul 3 5]))
        assertEqual "2 / (3 - 3)"        (runES (eval (2 / (3 - 3))) []) (Error DivideByZero) 
        assertEqual "11 / (10 - 5 * 2))" (runES (eval (11 / (10 - 5 * 2))) []) (Error DivideByZero)
  ]


main :: IO ()
main = defaultMain parserTests

