{-# LANGUAGE FlexibleContexts #-}

module Parser (Stack, Token(..), burlesque) where

import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Text.Parsec
import Text.Printf (printf)

data Token = BInt Int
           | BAdd
           | BFloat Double
           | BString [Char]
             deriving Eq


instance Show Token where
    show (BInt i) = show i
    show BAdd = ".+"
    show (BFloat d) = printf "%f" d
    show (BString s) = "\"" ++ s ++ "\""

(>|) :: Functor f => f a -> b -> f b
fa >| b = fmap (const b) fa

(<++>) :: Applicative a => a [b] -> a [b] -> a [b]
(<++>) = liftA2 (++)

num :: Stream s m Char => ParsecT s u m Token
num = comb <$> prefix <*> suffix
    where
      prefix = option "" (string "-") <++> many1 digit
      suffix = optionMaybe $ string "." <++> many1 digit
      comb i Nothing = BInt . read $ i
      comb i (Just f) = BFloat . read $ i ++ f

add :: Stream s m Char => ParsecT s u m Token
add = string ".+" >| BAdd

str :: Stream s m Char => ParsecT s u m Token
str = BString <$> between (char '"') (char '"') (many $ noneOf "\"")

sp :: Stream s m Char => ParsecT s u m ()
sp = void $ char ' '

type Stack = [Token]

burlesque :: Stream s m Char => ParsecT s u m Stack
burlesque = (tok `sepBy` sp) <* eof
    where tok = choice [num, add, str]
