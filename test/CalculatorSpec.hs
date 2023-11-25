module CalculatorSpec where

import Calculator
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import Options.Applicative (ParserResult(Success))
import Options.Applicative (ParserResult(Failure))

spec :: Spec
spec = describe "Calculator" $ do

     it "1 is 1" $
        1 `shouldBe` 1

     prop "is able to parse subcommand (verbose flag off)" $
        \as -> (reverse . reverse) as == (as :: [Int])
