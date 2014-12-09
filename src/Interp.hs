module Interp (interp) where

import Control.Arrow (left)
import Parser (Stack, Token(..))
import Text.Printf (printf)

interp :: Stack -> Either String Stack
interp [] = Left "empty stack"
interp (BAdd:rest) = add rest
interp (BReverse:rest) = rev rest
interp (BBlockAccess:rest) = blockAccess rest
interp (BExplode:rest) = explode rest
interp (BLength:rest) = len rest
interp (BSwap:rest) = swap rest
interp (BDrop:rest) = drp rest
interp (BDup:rest) = dup rest
interp (BAppend:rest) = append rest
interp (BPrepend:rest) = prepend rest
interp x = Right x

getarg :: Stack -> Either String (Token, Stack)
getarg stack = do
  result <- interp stack
  case result of
    (h:t) -> Right (h,t)
    _ -> Left "empty stack"

getarg2 :: Stack -> Either String (Token, Token, Stack)
getarg2 stack = do
  (a,t) <- getarg stack
  (b,t') <- getarg t
  return $ (a,b,t')

invalid a = Left $ printf "invalid operand %s" (show a)
invalid2 a b = Left $ printf "invalid operands %s %s" (show a) (show b)

add :: Stack -> Either String Stack
add stack = left (".+: " ++) $ do
  (a,b,t) <- getarg2 stack
  case (a,b) of
    ((BInt x), (BInt y)) -> Right $ BInt (x+y) : t
    ((BFloat x), (BFloat y)) -> Right $ BFloat (x+y) : t
    ((BString x), (BString y)) -> Right $ BString (y++x) : t
    ((BBlock x), (BBlock y)) -> Right $ BBlock (y++x) : t
    (x, y) -> invalid2 y x

rev :: Stack -> Either String Stack
rev stack = left ("<-: " ++) $ do
  (h,t) <- getarg stack
  case h of
    BString s -> return $ BString (reverse s) : t
    BBlock ts -> return $ BBlock (reverse ts) : t
    x -> invalid x

blockAccess :: Stack -> Either String Stack
blockAccess stack = left ("!!: " ++) $ do
  (ix,str,t) <- getarg2 stack
  go ix str t
    where
      go (BInt ix') (BString str') t
          | ix' < 0 || ix' >= length str' = Left $ printf "invalid index: %s" (show ix')
          | otherwise = Right $ BChar (str' !! ix') : t
      go ix str _ = invalid2 str ix

explode :: Stack -> Either String Stack
explode stack = left ("XX: " ++) $ do
  (h,t) <- getarg stack
  case h of
    BString s -> return $ BBlock (fmap BChar s) : t
    x -> invalid x

len :: Stack -> Either String Stack
len stack = left ("L[: " ++) $ do
  (h,t) <- getarg stack
  case h of
    BBlock b -> return $ BInt (length b) : t
    x -> invalid x

swap :: Stack -> Either String Stack
swap stack = left ("\\/: " ++) $ do
  (a,b,t) <- getarg2 stack
  return $ b:a:t

drp :: Stack -> Either String Stack
drp stack = left ("vv: " ++) $ do
  (_,t) <- getarg stack
  return t

dup :: Stack -> Either String Stack
dup stack = left ("vv: " ++) $ do
  (a,t) <- getarg stack
  return $ a:a:t

append :: Stack -> Either String Stack
append stack = left ("[+: " ++) $ do
  (a,b,t) <- getarg2 stack
  case b of
    BBlock ts -> return $ BBlock (ts ++ [a]) : t
    x -> invalid x

prepend :: Stack -> Either String Stack
prepend stack = left ("+]: " ++) $ do
  (a,b,t) <- getarg2 stack
  case b of
    BBlock ts -> return $ BBlock (a:ts) : t
    x -> invalid x
