module ParserSpec (spec) where

import Parser
import Test.Hspec
import Test.HUnit (assertFailure)
import Test.QuickCheck (property)
import Text.Parsec.Prim (runP)

shouldBeRight :: (Eq b, Show b) => Either a b -> b -> Expectation
shouldBeRight (Right a) e = a `shouldBe` e
shouldBeRight _ _ = assertFailure "not right"

shouldBeAnyLeft :: Either a b -> Expectation
shouldBeAnyLeft (Left _) = return ()
shouldBeAnyLeft _ = assertFailure "not a left"

spec = do
  describe "burlesque" $ do
    it "can parse integers" $ property $
       \x -> runP burlesque () "" (show (x :: Int)) `shouldBeRight` x
    it "fails to parse with trailing junk" $ property $
       \x -> shouldBeAnyLeft $ runP burlesque () "" (show (x :: Int) ++ ".")
                         
