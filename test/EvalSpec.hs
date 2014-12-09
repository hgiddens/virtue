module EvalSpec (spec) where

import Test.Hspec
import Text.Parsec (parse)
import Text.Printf (printf)

import qualified Eval

eval = Eval.eval "<test>"

evaluatesTo i o = it (printf "%s ⇒ %s" i o) $
                     eval i `shouldBe` Right o
                    
    

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
