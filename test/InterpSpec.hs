{-# LANGUAGE ScopedTypeVariables #-}
module InterpSpec (spec) where

import Test.Hspec
import Test.QuickCheck (property)

import Interp
import Parser (Token(..))
import TestUtils

spec = do
  describe "interpreter" $ do
    it "should evaluate an empty stack as itself" $
      interp [] `shouldBe` Right []
    describe "integers" $ do
      it "should evaluate a number as itself" $ property $
        \(x :: Int) -> interp [BInt x] `shouldBe` Right [BInt x]
    describe "addition" $ do
      it "should evaluate addition of two numbers" $ property $
        \(x :: Int, y :: Int) -> interp [BInt x, BInt y, BAdd] `shouldBe` Right [BInt (x+y)]
      it "should evaluate recursively" $ property $
        \(x :: Int, y :: Int, z :: Int) ->
            interp [BInt x, BInt y, BInt z, BAdd, BAdd] `shouldBe` Right [BInt (x+y+z)]
      it "should fail lacking sufficient operands" $
        shouldBeAnyLeft $ interp [BAdd]
