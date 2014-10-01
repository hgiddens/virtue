{-# LANGUAGE ScopedTypeVariables #-}
module InterpSpec (spec) where

import Control.Applicative
import Test.Hspec
import Test.QuickCheck -- (property)

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
      it "should evaluate addition of two integers" $ property $
        \(x :: Int, y :: Int) -> interp [BInt x, BInt y, BAdd] `shouldBe` Right [BInt (x+y)]
      it "should evaluate addition of two floats" $ property $
        \(x :: Double, y :: Double) ->
            interp [BFloat x, BFloat y, BAdd] `shouldBe` Right [BFloat (x+y)]
      it "should evaluate recursively" $ property $
        \(x :: Int, y :: Int, z :: Int) ->
            interp [BInt x, BInt y, BInt z, BAdd, BAdd] `shouldBe` Right [BInt (x+y+z)]
      it "should fail lacking sufficient operands" $
        shouldBeAnyLeft $ interp [BAdd]
      it "should fail presented mixed operands" $
        shouldBeAnyLeft $ interp [BFloat 1.0, BInt 1, BAdd]
    describe "reversing" $ do
      it "should reverse a string" $ property $
         \s -> interp [BString s, BReverse] `shouldBe` Right [BString (reverse s)]
      it "should be the identity function applied twice" $ property $
         \s -> interp [BString s, BReverse, BReverse] `shouldBe` Right [BString s]
      it "should fail lacking an operand" $
         shouldBeAnyLeft $ interp [BReverse]
      it "should fail given a non-string operand" $ property $
         let notString (BString _) = False
             notString _ = True
             ffmap = flip fmap
         in (arbitrary `suchThat` notString) `ffmap` \token ->
             shouldBeAnyLeft $ interp [token, BReverse]
