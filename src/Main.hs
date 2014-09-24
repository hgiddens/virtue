import Control.Applicative
import Control.Monad
import Data.List
import Parser (Stack, burlesque)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec (parse)

err :: Show s => s -> IO ()
err s = hPutStrLn stderr (show s) >> exitFailure

out :: Stack -> IO ()
out = putStrLn . join . intersperse " " . map show

main :: IO ()
main = do
  program <- parse burlesque "<stdin>" <$> getContents
  either err out program
