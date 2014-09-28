import Control.Arrow (left)
import Control.Monad
import Data.List
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec (parse)

import Interp (interp)
import Parser (Stack, burlesque)

err :: String -> IO ()
err s = hPutStrLn stderr ("error: " ++ s) >> exitFailure

out :: Stack -> IO ()
out = putStrLn . join . intersperse " " . map show

eval :: String -> Either String Stack
eval input = do
  stack <- left show $ parse burlesque "<stdin>" input
  interp stack

main :: IO ()
main = do
  input <- getContents
  either err out $ eval input
