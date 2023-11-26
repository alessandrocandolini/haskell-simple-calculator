module CalculatorSpec where

import Calculator
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

example1 :: Ast
example1 = add (multiply (add (value 2) (value 3)) (value 4)) (value 5)

spec :: Spec
spec = describe "Calculator" $ do

     it "simple evaluate" $
        evaluate example1 `shouldBe` 25

     it "simple pretty" $
        pretty example1 `shouldBe` "(((2+3)*4)+5)"

     it "simple pretty" $ let
        original :: Ast
        original = add (multiply (add (value 0) (value 2)) (value 1)) (value 5)
       in
        simplify original `shouldBe` add (value 2) (value 5)
