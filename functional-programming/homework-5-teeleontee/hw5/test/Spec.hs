import Test.Tasty
import Test.Tasty.HUnit

import HW5.Parser
import HW5.Base
-- import HW5.Action
-- import HW5.Evaluator (eval)

parserTests :: TestTree
parserTests = testGroup
  "Tests for Homework 5"
 [
  testCase "T1 - parse smoke" $
    do 
      assertEqual "add(1, 2)" 
        (Right (HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) [HiExprValue (HiValueNumber (1 :: Rational)),HiExprValue (HiValueNumber (2 :: Rational))]))
          (parse "add(1, 2)")
      assertEqual "sub"
        (Right (HiExprValue (HiValueFunction HiFunSub)))
          (parse "sub")
      assertEqual "123"
        (Right (HiExprValue (HiValueNumber (123 :: Rational))))
          (parse "123"),
   testCase "T1 - parse whitespace" $
     do
       assertEqual "add(   1,   2   )"
        (Right (HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) [HiExprValue (HiValueNumber (1 :: Rational)),HiExprValue (HiValueNumber (2 :: Rational))]))
          (parse "add(   1,   2   )")
       assertEqual "    add(    1   ,    2   ,   3  )(   123  )    " 
         (Right 
           (HiExprApply (HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) 
             [HiExprValue (HiValueNumber (1 :: Rational))
             ,HiExprValue (HiValueNumber (2 :: Rational))
             ,HiExprValue (HiValueNumber (3 :: Rational))
             ])
           [HiExprValue (HiValueNumber (123 :: Rational))
           ]))
          (parse "    add(    1   ,    2   ,   3  )(   123  )    ")
       assertEqual " (    123   )  "
         (Right (HiExprValue (HiValueNumber (123 :: Rational))))
           (parse " (    123   )  "),
    testCase "T1 - parse basic expressions" $
      do
        assertEqual "sub(1, 2, 3, 4)"
          (Right 
            (HiExprApply (HiExprValue (HiValueFunction HiFunSub)) 
              [HiExprValue (HiValueNumber (1 :: Rational))
              ,HiExprValue (HiValueNumber (2 :: Rational))
              ,HiExprValue (HiValueNumber (3 :: Rational))
              ,HiExprValue (HiValueNumber (4 :: Rational))]))
            (parse "sub(1, 2, 3, 4)")
        assertEqual "add(1, 2, sub(3, 4), 8)(sub(1, 0))"
         (Right (HiExprApply (HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) 
           [HiExprValue (HiValueNumber (1 :: Rational))
           ,HiExprValue (HiValueNumber (2 :: Rational))
           ,HiExprApply (HiExprValue (HiValueFunction HiFunSub)) 
             [HiExprValue (HiValueNumber (3 :: Rational))
             ,HiExprValue (HiValueNumber (4 :: Rational))]
             ,HiExprValue (HiValueNumber (8 :: Rational))]) 
               [HiExprApply (HiExprValue (HiValueFunction HiFunSub)) 
                 [HiExprValue (HiValueNumber (1 :: Rational))
                 ,HiExprValue (HiValueNumber (0 :: Rational))]]))
           (parse "add(1, 2, sub(3, 4), 8)(sub(1, 0))")
        assertEqual "add(1, 2, 3)(3, 5)(sub(1, 2))"
          (Right (HiExprApply (HiExprApply (HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) 
            [HiExprValue (HiValueNumber (1 :: Rational)),HiExprValue (HiValueNumber (2 :: Rational)),HiExprValue (HiValueNumber (3 :: Rational))]) 
              [HiExprValue (HiValueNumber (3 :: Rational)),HiExprValue (HiValueNumber (5 :: Rational))]) 
                [HiExprApply (HiExprValue (HiValueFunction HiFunSub)) 
                  [HiExprValue (HiValueNumber (1 :: Rational)),
                  HiExprValue (HiValueNumber (2 :: Rational))]]))
            (parse "add(1, 2, 3)(3, 5)(sub(1, 2))"),
   testCase "T1 - parse numbers" $
     do
       assertEqual "-1.618"
         (Right (HiExprValue (HiValueNumber ((-809) / 500))))
           (parse "-1.618")
       assertEqual "-1.618618618"
         (Right (HiExprValue (HiValueNumber ((-809309309) / 500000000))))
           (parse "-1.618618618")
       assertEqual "-1.123123123123"
         (Right (HiExprValue (HiValueNumber ((-1123123123123) / 1000000000000))))
           (parse "-1.123123123123")
    -- I am sorry, I am burned out, I don't want to do this anymore
 ]

main :: IO ()
main = defaultMain parserTests
