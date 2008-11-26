import Mbox (unMbox,parseMbox)
import qualified Data.ByteString.Lazy as B
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do hPutStrLn stderr "Reading from stdin..."
          (print . length . unMbox . parseMbox) =<< B.getContents
