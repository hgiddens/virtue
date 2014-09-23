{-# LANGUAGE FlexibleContexts #-}

module Parser (burlesque) where

import Control.Applicative
import Text.Parsec

(>|) :: Functor f => f a -> b -> f b
fa >| b = fmap (const b) fa

integer :: Stream s m Char => ParsecT s u m Int
integer = option id (char '-' >| negate) <*> fmap read (many1 digit)

type Burlesque = Int

burlesque :: Stream s m Char => ParsecT s u m Burlesque
burlesque = integer <* eof
