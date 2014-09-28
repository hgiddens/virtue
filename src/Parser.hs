{-# LANGUAGE FlexibleContexts #-}

module Parser (Stack, Token(..), burlesque) where

import Control.Applicative
import Control.Monad
import Text.Parsec

data Token = BInt Int deriving Eq

instance Show Token where
    show (BInt i) = show i

(>|) :: Functor f => f a -> b -> f b
fa >| b = fmap (const b) fa

integer :: Stream s m Char => ParsecT s u m Token
integer = BInt <$> (option id (char '-' >| negate) <*> fmap read (many1 digit))

sp :: Stream s m Char => ParsecT s u m ()
sp = void $ char ' '

type Stack = [Token]

burlesque :: Stream s m Char => ParsecT s u m Stack
burlesque = (integer `sepBy` sp) <* eof
