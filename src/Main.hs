import Control.Applicative
import Parser (burlesque)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec (parse)

err :: Show s => s -> IO ()
err s = hPutStrLn stderr (show s) >> exitFailure

main :: IO ()
main = do
  program <- parse burlesque "<stdin>" <$> getContents
  either err (putStrLn . show) program
