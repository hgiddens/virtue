module LibSpec (spec) where

import Test.Hspec
import Lib

spec = do
  describe "absolute" $ do
         it "returns the original number given positive" $
            absolute 1 `shouldBe` 1
         it "returns the positive number given negative" $
            absolute (-1) `shouldBe` 1
         it "returns zero given zero" $
            absolute 0 `shouldBe` 0
