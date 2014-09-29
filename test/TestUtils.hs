module TestUtils (shouldBeRight, shouldBeAnyLeft) where

import Control.Applicative ((<$>))
import Test.Hspec (Expectation, shouldBe)
import Test.HUnit (assertFailure)
import Test.QuickCheck (Arbitrary, arbitrary, oneof, shrink, suchThat)

import Parser

instance Arbitrary Token where
    arbitrary = oneof [BInt <$> arbitrary,
                       return BAdd,
                       BFloat <$> arbitrary,
                       BString <$> arbitrary `suchThat` noescape
                      ]
        -- TODO improve me! "foo\bar" should be valid
        where noescape :: String -> Bool
              noescape s = not $ any (=='\\') (show s)
    shrink _ = []

shouldBeRight :: (Eq b, Show a, Show b) => Either a b -> b -> Expectation
shouldBeRight (Right a) e = a `shouldBe` e
shouldBeRight (Left l) _ = assertFailure $ "not right : Left " ++ (show l)

shouldBeAnyLeft :: Either a b -> Expectation
shouldBeAnyLeft (Left _) = return ()
shouldBeAnyLeft _ = assertFailure "not left"
