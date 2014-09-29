module Interp (interp) where

import Control.Arrow (left)
import Parser (Stack, Token(..))
import Text.Printf (printf)

interp :: Stack -> Either String Stack
interp [] = Right []
interp stack = fmap reverse . interp' . reverse $ stack

interp' :: Stack -> Either String Stack
interp' [] = Left "empty stack"
interp' (BAdd:rest) = add rest
interp' x = Right x

add :: Stack -> Either String Stack
add stack = left (".+: " ++) $ do
  (a:t) <- interp' stack
  (b:t') <- interp' t
  case (a,b) of
    ((BInt x), (BInt y)) -> Right $ BInt (x+y) : t'
    ((BFloat x), (BFloat y)) -> Right $ BFloat (x+y) : t'
    (x, y) -> Left $ printf "invalid operands: %s %s" (show y) (show x)
