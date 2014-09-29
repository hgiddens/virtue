module ParserSpec (spec) where

import Data.List (intersperse)
import Test.Hspec
import Test.HUnit (assertFailure)
import Test.QuickCheck (property)
import Text.Parsec.Prim (runP)

import Parser
import TestUtils

input :: [Token] -> String
input = foldr (++) "" . intersperse " " . map show

spec = do
  describe "burlesque" $ do
    it "can parse tokens" $ property $
       \l -> runP burlesque () "" (input l) `shouldBeRight` l
    it "fails to parse with trailing junk" $ property $
       \l -> shouldBeAnyLeft $ runP burlesque () "" (input l ++ "X")
