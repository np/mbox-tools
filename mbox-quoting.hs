import Codec.Mbox (fromQuoting)
import qualified Data.ByteString.Lazy.Char8 as C (getContents,putStr)
import Control.Applicative ((<$>))
import System.Environment (getArgs)
import System.IO (hPutStrLn,stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["quot"]   -> C.putStr =<< fromQuoting (+1) <$> C.getContents
    ["unquot"] -> C.putStr =<< fromQuoting (+(-1)) <$> C.getContents
    _          -> hPutStrLn stderr "Usage: mbox-quoting {quot|unquot}"