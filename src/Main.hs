module Main (main, eval) where

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Eval (eval)

err :: String -> IO ()
err s = hPutStrLn stderr s >> exitFailure

out :: String -> IO ()
out = putStrLn

main :: IO ()
main = do
  input <- getContents
  either err out $ eval "<stdin>" input
