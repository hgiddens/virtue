module Eval (eval) where

import Control.Arrow (left)
import Control.Monad
import Data.List
import Text.Parsec (parse)

import Parser (Stack, burlesque)
import Interp (interp)

err :: String -> String
err = (++) "error: "

out :: Stack -> String
out = join . intersperse " " . map show

bimap :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
bimap f _ (Left a) = Left (f a)
bimap _ f (Right b) = Right (f b)

eval :: String -> String -> Either String String
eval name input = bimap err out $ do
  stack <- left show $ parse burlesque name input
  interp stack
