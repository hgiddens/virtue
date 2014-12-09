{-# LANGUAGE FlexibleContexts #-}

module Parser (Stack, Token(..), burlesque) where

import Control.Applicative hiding ((<|>), many)
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
           | BExplode
           | BLength
           | BSwap
           | BDrop
           | BDup
             deriving Eq


instance Show Token where
    show (BInt i) = show i
    show (BFloat d) = printf "%f" d
    show (BString s) = "\"" ++ s ++ "\""
    show (BChar c) = '\'':c:[]
    show BAdd = ".+"
    show BReverse = "<-"
    show BBlockAccess = "!!"
    show BExplode = "XX"
    show BLength = "L["
    show BSwap = "\\/"
    show BDrop = "vv"
    show BDup = "^^"
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
chr = BChar <$> (char '\'' *> anyChar)

add :: Stream s m Char => ParsecT s u m Token
add = BAdd <$ string ".+"

rev :: Stream s m Char => ParsecT s u m Token
rev = BReverse <$ string "<-"

blockAccess :: Stream s m Char => ParsecT s u m Token
blockAccess = BBlockAccess <$ string "!!"

block :: Stream s m Char => ParsecT s u m Token
block = fmap BBlock $ between prefix suffix toks
    where prefix = char '{' *> sp
          suffix = sp <* char '}' 

explode :: Stream s m Char => ParsecT s u m Token
explode = BExplode <$ string "XX"

len :: Stream s m Char => ParsecT s u m Token
len = BLength <$ string "L["

swap :: Stream s m Char => ParsecT s u m Token
swap = BSwap <$ string "\\/"

drp :: Stream s m Char => ParsecT s u m Token
drp = BDrop <$ string "vv"

dup :: Stream s m Char => ParsecT s u m Token
dup = BDup <$ string "^^"

sp :: Stream s m Char => ParsecT s u m ()
sp = skipMany $ char ' '

type Stack = [Token]

toks :: Stream s m Char => ParsecT s u m [Token]
toks = tok `sepBy` sp
    where tok = choice [num, str, chr, add, rev, blockAccess, block, explode, len, swap, drp, dup]

burlesque :: Stream s m Char => ParsecT s u m Stack
burlesque = reverse <$> toks <* eof
