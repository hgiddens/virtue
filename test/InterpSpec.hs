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

commandp (BInt _) = False
commandp (BFloat _) = False
commandp (BString _) = False
commandp (BChar _) = False
commandp _ = True

spec = do
  describe "interpreter" $ do
    it "should evaluate an empty stack as an error" $
      shouldBeAnyLeft $ interp []
    describe "integers" $ do
      it "should evaluate a number as itself" $ property $
        \(x :: Int) -> interp [BInt x] `shouldBe` Right [BInt x]
    describe "addition" $ do
      it "should evaluate addition of two integers" $ property $
        \(x :: Int, y :: Int) -> interp [BAdd, BInt x, BInt y] `shouldBe` Right [BInt (x+y)]
      it "should evaluate addition of two floats" $ property $
        \(x :: Double, y :: Double) ->
            interp [BAdd, BFloat x, BFloat y] `shouldBe` Right [BFloat (x+y)]
      it "should evaluate addition of two strings" $ property $
        \(x :: String, y :: String) ->
            interp [BAdd, BString x, BString y] `shouldBe` Right [BString (y++x)]
      it "should evaluate concatenation of two blocks" $ property $
        \x y -> interp [BAdd, BBlock x, BBlock y] `shouldBe` Right [BBlock (y++x)]
      it "should evaluate recursively" $ property $
        \(x :: Int, y :: Int, z :: Int) ->
            interp [BAdd, BAdd, BInt x, BInt y, BInt z] `shouldBe` Right [BInt (x+y+z)]
      it "should fail lacking sufficient operands" $
        shouldBeAnyLeft $ interp [BAdd]
      it "should fail presented mixed operands" $
        shouldBeAnyLeft $ interp [BAdd, BFloat 1.0, BInt 1]
    describe "reversing" $ do
      describe "on strings" $ do
        it "should reverse a string" $ property $
           \s -> interp [BReverse, BString s] `shouldBe` Right [BString (reverse s)]
        it "should be the identity function applied twice" $ property $
           \s -> interp [BReverse, BReverse, BString s] `shouldBe` Right [BString s]
      describe "on blocks" $ do
        it "should reverse a block" $ property $
           \ts -> interp [BReverse, BBlock ts] `shouldBe` Right [BBlock (reverse ts)]
      it "should fail lacking an operand" $
         shouldBeAnyLeft $ interp [BReverse]
      it "should fail given a non-string operand" $ property $
         let notReversable (BString _) = False
             notReversable (BBlock _) = False
             notReversable _ = True
         in (arbitrary `suchThat` notReversable) `ffmap` \token ->
             shouldBeAnyLeft $ interp [BReverse, token]
    describe "block access" $ do
      it "should index a string successfully" $ property $
         let foo = do string <- (arbitrary :: Gen String) `suchThat` (not . null)
                      index <- choose (0, pred $ length string)
                      return (string, index)
         in foo `ffmap` \(str, ix) ->
             interp [BBlockAccess, BInt ix, BString str] `shouldBeRight` [BChar (str !! ix)]
      it "should fail indexing past an end of a string" $ property $
         let foo = do str <- arbitrary :: Gen String
                      ix <- (arbitrary :: Gen Int) `suchThat` (\i -> i < 0 || i >= (length str))
                      return (str,ix)
         in foo `ffmap` \(str, ix) ->
             shouldBeAnyLeft $ interp [BBlockAccess, BInt ix, BString str]
      it "should fail given a non-string" $ property $
         do str <- (arbitrary :: Gen Token) `suchThat` (\t -> case t of (BString _) -> False; _ -> True)
            ix <- BInt <$> arbitrary
            return $ shouldBeAnyLeft $ interp [BBlockAccess, ix, str]
      it "should fail given a non-int" $ property $
         do str <- BString <$> arbitrary
            ix <- (arbitrary :: Gen Token) `suchThat` (\t -> case t of (BInt _) -> False; _ -> True)
            return $ shouldBeAnyLeft $ interp [BBlockAccess, ix, str]
      it "should fail lacking sufficient arguments" $
         shouldBeAnyLeft $ interp [BBlockAccess]
    describe "explode" $ do
      it "should explode a string" $ property $
         \s -> interp [BExplode, BString s] `shouldBe` Right [BBlock (fmap BChar s)]
    describe "length" $ do
      it "should describe the length of a block" $ property $
         \b -> interp [BLength, BBlock b] `shouldBe` Right [BInt (length b)]
    describe "swap" $ do
      it "should swap the two items on the top of the stack" $ property $
         do a <- arbitrary `suchThat` (not . commandp)
            b <- arbitrary `suchThat` (not . commandp)
            t <- arbitrary
            return $ interp (BSwap:a:b:t) `shouldBe` Right (b:a:t)
      it "should accomodate commands" $
         interp [BSwap, BAdd, BInt 1, BInt 2, BInt 4] `shouldBe` Right [BInt 4, BInt 3]
    describe "drop" $ do
      it "should drop the top item on the stack" $ property $
         do a <- arbitrary `suchThat` (not . commandp)
            t <- arbitrary
            return $ interp (BDrop:a:t) `shouldBe` Right t
    describe "dup" $ do
      it "should dup the top item on the stack" $ property $
         do a <- arbitrary `suchThat` (not . commandp)
            t <- arbitrary
            return $ interp (BDup:a:t) `shouldBe` Right (a:a:t)
