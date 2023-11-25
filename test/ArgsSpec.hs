module ArgsSpec where

import Args
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import Options.Applicative (ParserResult(Success))
import Options.Applicative (ParserResult(Failure))

parseArgsMaybe = transform . parseArgs where
  transform (Success a) = Right a
  transform (Failure failure) = Left (show failure)
  transform _ = Left "completion"

spec :: Spec
spec = describe "Args parser" $ do

     it "is able to parse subcommand (verbose flag on)" $
        parseArgsMaybe ["-v", "doctor"] `shouldBe` Right (Args (Flags Verbose) Doctor)

     it "is able to parse subcommand (verbose flag off)" $
        parseArgsMaybe ["doctor"] `shouldBe` Right (Args (Flags NotVerbose) (Doctor))
