module ParserSpec (spec) where

import Control.Applicative
import Data.List (intersperse)
import Parser
import Test.Hspec
import Test.HUnit (assertFailure)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, property, shrink)
import Text.Parsec.Prim (runP)

shouldBeRight :: (Eq b, Show b) => Either a b -> b -> Expectation
shouldBeRight (Right a) e = a `shouldBe` e
shouldBeRight _ _ = assertFailure "not right"

shouldBeAnyLeft :: Either a b -> Expectation
shouldBeAnyLeft (Left _) = return ()
shouldBeAnyLeft _ = assertFailure "not left"

data TToken = TInt Int

instance Show TToken where
    show (TInt i) = show i

instance Arbitrary TToken where
    arbitrary = TInt <$> arbitrary
    shrink _ = []

expected :: [TToken] -> [Int]
expected = map f
    where f (TInt i) = i

input :: [TToken] -> String
input = foldr (++) "" . intersperse " " . map show

spec = do
  describe "burlesque" $ do
    it "can parse tokens" $ property $
       \l -> runP burlesque () "" (input l) `shouldBeRight` expected l
    it "fails to parse with trailing junk" $ property $
       \l -> shouldBeAnyLeft $ runP burlesque () "" (input l ++ ".")
