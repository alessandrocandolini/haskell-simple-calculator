module CalculatorSpec where

import Calculator
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

example1 :: Ast
example1 = add (multiply (add (value 2) (value 3)) (value 4)) (value 5)

printed1 :: String
printed1 = "(((2+3)*4)+5)"

tokenised1 :: [Token]
tokenised1 = [Open, Open, Open, Val 2, Plus, Val 3, Close, Times, Val 4, Close, Plus, Val 5, Close]

spec :: Spec
spec = describe "Calculator" $ do
  it "simple evaluate"
    $ evaluate example1
    `shouldBe` 25

  it "simple pretty"
    $ pretty example1
    `shouldBe` "(((2+3)*4)+5)"

  it "simplify example 1"
    $ let
        original :: Ast
        original = add (multiply (add (value 0) (value 2)) (value 1)) (value 5)
       in
        simplify original `shouldBe` add (value 2) (value 5)

  it "simplify example 2"
    $ let
        original :: Ast
        original = add (multiply (add (value 1) (value 0)) (value 2)) (value 5)
       in
        simplify original `shouldBe` add (value 2) (value 5)

  it "simplify example 3"
    $ let
        original :: Ast
        original = add (multiply (add (value 1) (value 0)) (value 5)) (value 0)
       in
        simplify original `shouldBe` value 5

  it "parse tokens"
    $ parseTokens printed1
    `shouldBe` Right tokenised1

  xit "parse ast successfully"
    $ parseAst tokenised1 `shouldBe` Just example1
