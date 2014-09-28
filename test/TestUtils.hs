module TestUtils (shouldBeRight, shouldBeAnyLeft) where

import Control.Applicative ((<$>))
import Test.Hspec (Expectation, shouldBe)
import Test.HUnit (assertFailure)
import Test.QuickCheck (Arbitrary, arbitrary, oneof, shrink)

import Parser

instance Arbitrary Token where
    arbitrary = oneof [BInt <$> arbitrary, return BAdd]
    shrink _ = []

shouldBeRight :: (Eq b, Show b) => Either a b -> b -> Expectation
shouldBeRight (Right a) e = a `shouldBe` e
shouldBeRight _ _ = assertFailure "not right"

shouldBeAnyLeft :: Either a b -> Expectation
shouldBeAnyLeft (Left _) = return ()
shouldBeAnyLeft _ = assertFailure "not left"
