{-# LANGUAGE QuasiQuotes #-}
module CalculatorParserSpec where

import Calculator
import CalculatorParser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

example1 :: Ast Int
example1 = add (multiply (add (value 2) (value 3)) (value 4)) (value 5)

spec :: Spec
spec = describe "CalculatorParser" $ do
  it "simple evaluate"
    $ evaluate example1
    `shouldBe` 25

