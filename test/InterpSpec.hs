{-# LANGUAGE ScopedTypeVariables #-}
module InterpSpec (spec) where

import Control.Applicative
import Test.Hspec
import Test.QuickCheck -- (property)

import Interp
import Parser (Token(..))
import TestUtils

ffmap :: Functor f => f a -> (a -> b) -> f b
ffmap = flip fmap

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
      it "should evaluate addition of two strings" $ property $
        \(x :: String, y :: String) ->
            interp [BString x, BString y, BAdd] `shouldBe` Right [BString (x++y)]
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
         in (arbitrary `suchThat` notString) `ffmap` \token ->
             shouldBeAnyLeft $ interp [token, BReverse]
    describe "block access" $ do
      it "should index a string successfully" $ property $
         let foo = do string <- (arbitrary :: Gen String) `suchThat` (not . null)
                      index <- choose (0, pred $ length string)
                      return (string, index)
         in foo `ffmap` \(str, ix) ->
             interp [BString str, BInt ix, BBlockAccess] `shouldBeRight` [BChar (str !! ix)]
      it "should fail indexing past an end of a string" $ property $
         let foo = do str <- arbitrary :: Gen String
                      ix <- (arbitrary :: Gen Int) `suchThat` (\i -> i < 0 || i >= (length str))
                      return (str,ix)
         in foo `ffmap` \(str, ix) ->
             shouldBeAnyLeft $ interp [BString str, BInt ix, BBlockAccess]
      it "should fail given a non-string" $ property $
         do str <- (arbitrary :: Gen Token) `suchThat` (\t -> case t of (BString _) -> False; _ -> True)
            ix <- BInt <$> arbitrary
            return $ shouldBeAnyLeft $ interp [str, ix, BBlockAccess]
      it "should fail given a non-int" $ property $
         do str <- BString <$> arbitrary
            ix <- (arbitrary :: Gen Token) `suchThat` (\t -> case t of (BInt _) -> False; _ -> True)
            return $ shouldBeAnyLeft $ interp [str, ix, BBlockAccess]
      it "should fail lacking sufficient arguments" $
         shouldBeAnyLeft $ interp [BBlockAccess]
