{-# LANGUAGE FlexibleContexts #-}

module Parser (Stack, Token(..), burlesque) where

import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Data.List (intersperse)
import Data.Monoid (mconcat)
import Text.Parsec
import Text.Printf (printf)

data Token = BInt Int
           | BFloat Double
           | BString [Char]
           | BChar Char
           | BAdd
           | BReverse
           | BBlock [Token]
           | BBlockAccess
             deriving Eq


instance Show Token where
    show (BInt i) = show i
    show (BFloat d) = printf "%f" d
    show (BString s) = "\"" ++ s ++ "\""
    show (BChar c) = '\'':c:[]
    show BAdd = ".+"
    show BReverse = "<-"
    show BBlockAccess = "!!"
    show (BBlock ts) = "{" ++ (mconcat $ intersperse " " $ map show ts) ++ "}"

(<++>) :: Applicative a => a [b] -> a [b] -> a [b]
(<++>) = liftA2 (++)

num :: Stream s m Char => ParsecT s u m Token
num = comb <$> prefix <*> suffix
    where
      prefix = option "" (string "-") <++> many1 digit
      suffix = optionMaybe $ string "." <++> many1 digit
      comb i Nothing = BInt . read $ i
      comb i (Just f) = BFloat . read $ i ++ f

str :: Stream s m Char => ParsecT s u m Token
str = BString <$> between (char '"') (char '"') (many $ noneOf "\"")

chr :: Stream s m Char => ParsecT s u m Token
chr = BChar <$> (char '\'' >> anyChar)

add :: Stream s m Char => ParsecT s u m Token
add = BAdd <$ string ".+"

rev :: Stream s m Char => ParsecT s u m Token
rev = BReverse <$ string "<-"

blockAccess :: Stream s m Char => ParsecT s u m Token
blockAccess = BBlockAccess <$ string "!!"

block :: Stream s m Char => ParsecT s u m Token
block = fmap BBlock $ between (char '{') (char '}') toks

sp :: Stream s m Char => ParsecT s u m ()
sp = void $ char ' '

type Stack = [Token]

toks :: Stream s m Char => ParsecT s u m [Token]
toks = tok `sepBy` (option () sp)
    where tok = choice [num, str, chr, add, rev, blockAccess, block]

burlesque :: Stream s m Char => ParsecT s u m Stack
burlesque = toks <* eof
