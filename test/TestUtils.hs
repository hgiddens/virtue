module TestUtils (shouldBeRight, shouldBeAnyLeft) where

import Control.Applicative ((<$>))
import Test.Hspec (Expectation, shouldBe)
import Test.HUnit (assertFailure)
import Test.QuickCheck (Arbitrary, arbitrary, listOf, oneof, shrink, sized, suchThat)

import Parser

instance Arbitrary Token where
    arbitrary = sized gen
        where noquote :: String -> Bool
              noquote s = not (any (=='"') s)

              leaves = [BInt <$> arbitrary,
                        BFloat <$> arbitrary,
                        BString <$> arbitrary `suchThat` noquote,
                        BChar <$> arbitrary,
                        return BAdd,
                        return BReverse,
                        return BBlockAccess]

              gen 0 = oneof $ (BBlock <$> return []) : leaves
              gen n = oneof $ (BBlock <$> listOf (gen (n `div` 10))) : leaves

    shrink _ = []

shouldBeRight :: (Eq b, Show a, Show b) => Either a b -> b -> Expectation
shouldBeRight (Right a) e = a `shouldBe` e
shouldBeRight (Left l) _ = assertFailure $ "not right : Left " ++ (show l)

shouldBeAnyLeft :: Either a b -> Expectation
shouldBeAnyLeft (Left _) = return ()
shouldBeAnyLeft _ = assertFailure "not left"
