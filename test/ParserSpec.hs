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

data TLex = LInt Int

instance Show TLex where
    show (LInt i) = show i

instance Arbitrary TLex where
    arbitrary = LInt <$> arbitrary
    shrink _ = []

expected :: [TLex] -> [Int]
expected = map f
    where f (LInt i) = i

input :: [TLex] -> String
input = foldr (++) "" . intersperse " " . map show

spec = do
  describe "burlesque" $ do
    it "can parse lexemes" $ property $
       \l -> runP burlesque () "" (input l) `shouldBeRight` expected l
    it "fails to parse with trailing junk" $ property $
       \l -> shouldBeAnyLeft $ runP burlesque () "" (input l ++ ".")
