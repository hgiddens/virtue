module ParserSpec (spec) where

import Control.Applicative
import Data.List (intersperse)
import Parser
import Test.Hspec
import Test.HUnit (assertFailure)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, oneof, property, shrink)
import Text.Parsec.Prim (runP)

shouldBeRight :: (Eq b, Show b) => Either a b -> b -> Expectation
shouldBeRight (Right a) e = a `shouldBe` e
shouldBeRight _ _ = assertFailure "not right"

shouldBeAnyLeft :: Either a b -> Expectation
shouldBeAnyLeft (Left _) = return ()
shouldBeAnyLeft _ = assertFailure "not left"

instance Arbitrary Token where
    arbitrary = oneof [BInt <$> arbitrary, return BAdd]
    shrink _ = []

input :: [Token] -> String
input = foldr (++) "" . intersperse " " . map show

spec = do
  describe "burlesque" $ do
    it "can parse tokens" $ property $
       \l -> runP burlesque () "" (input l) `shouldBeRight` l
    it "fails to parse with trailing junk" $ property $
       \l -> shouldBeAnyLeft $ runP burlesque () "" (input l ++ ".")
