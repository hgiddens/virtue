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
interp' (BReverse:rest) = rev rest
interp' (BBlockAccess:rest) = blockAccess rest
interp' x = Right x

add :: Stack -> Either String Stack
add stack = left (".+: " ++) $ do
  (a:t) <- interp' stack
  (b:t') <- interp' t
  case (a,b) of
    ((BInt x), (BInt y)) -> Right $ BInt (x+y) : t'
    ((BFloat x), (BFloat y)) -> Right $ BFloat (x+y) : t'
    ((BString x), (BString y)) -> Right $ BString (y++x) : t'
    (x, y) -> Left $ printf "invalid operands: %s %s" (show y) (show x)

rev :: Stack -> Either String Stack
rev stack = left ("<-: " ++) $ do
  (h:t) <- interp' stack
  case h of
    BString s -> return $ BString (reverse s) : t
    BBlock ts -> return $ BBlock (reverse ts) : t
    x -> Left $ printf "invalid operand: %s" (show x)

blockAccess :: Stack -> Either String Stack
blockAccess stack = left ("!!: " ++) $ do
  (ix:t) <- interp' stack
  (str:t') <- interp' t
  go ix str t'
    where
      go (BInt ix') (BString str') t'
          | ix' < 0 || ix' >= length str' = Left $ printf "invalid index: %s" (show ix')
          | otherwise = Right $ BChar (str' !! ix') : t'
      go ix str _ = Left $ printf "invalid operands: %s %s" (show str) (show ix)
