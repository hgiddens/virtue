{-# LANGUAGE FlexibleContexts #-}

module Parser (Stack, Token(..), burlesque) where

import Control.Applicative hiding ((<|>))
import Control.Monad
import Text.Parsec

data Token = BInt Int
           | BAdd
             deriving Eq


instance Show Token where
    show (BInt i) = show i
    show BAdd = ".+"

(>|) :: Functor f => f a -> b -> f b
fa >| b = fmap (const b) fa

integer :: Stream s m Char => ParsecT s u m Token
integer = BInt <$> (option id (char '-' >| negate) <*> fmap read (many1 digit))

add :: Stream s m Char => ParsecT s u m Token
add = string ".+" >| BAdd

sp :: Stream s m Char => ParsecT s u m ()
sp = void $ char ' '

type Stack = [Token]

burlesque :: Stream s m Char => ParsecT s u m Stack
burlesque = (tok `sepBy` sp) <* eof
    where tok = integer <|> add
