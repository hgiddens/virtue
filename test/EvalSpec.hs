module EvalSpec (spec) where

import Data.Monoid (mconcat)
import Data.List (intersperse)
import Test.Hspec
import Text.Parsec (parse)
import Text.Printf (printf)

import qualified Eval

eval = Eval.eval "<test>"

evaluatesTo i o = it (printf "%s ⇒ %s" i (nl2comma o)) $
                     eval i `shouldBe` Right o
    where nl2comma ('\n':t) = ',':(nl2comma t)
          nl2comma (h:t) = h:(nl2comma t)
          nl2comma [] = []

spec = do
  describe "lesson one" $ do
         "5 5 .+" `evaluatesTo` "10"
         "5.0 5.0 .+" `evaluatesTo` "10.0"
         "\"Hello, world!\"" `evaluatesTo` "\"Hello, world!\""
         "\"Hello, world!\" <-" `evaluatesTo` "\"!dlrow ,olleH\""
         "\"a\" \"b\" .+" `evaluatesTo` "\"ab\""
         "'c" `evaluatesTo` "'c"
         "\"Hi\"0!!" `evaluatesTo` "'H"
         "\"Hi\"1!!" `evaluatesTo` "'i"
         "{1 2 3}" `evaluatesTo` "{1 2 3}"
         "{1 2 3}<-" `evaluatesTo` "{3 2 1}"
         "\"Hello\"XX" `evaluatesTo` "{'H 'e 'l 'l 'o}"
         "{{ 1 2}{3}}L[" `evaluatesTo` "2"
  describe "lesson two" $ do
         "1 2 3" `evaluatesTo` "3\n2\n1"
         "1 2 3\\/" `evaluatesTo` "2\n3\n1"
         "1 2 3vv" `evaluatesTo` "2\n1"
         "1 2 3^^" `evaluatesTo` "3\n3\n2\n1"
         -- NYI "100ro{3.%}f[" `evaluatesTo` ("{" ++ mconcat (intersperse " " [show i | i <- [0..100], i `mod` 3 /= 0]) ++ "}")
  describe "lesson three" $ do
         "{1 2 3}{4 5 6}.+" `evaluatesTo` "{1 2 3 4 5 6}"
