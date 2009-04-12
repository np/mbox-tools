import Codec.Mbox (mboxMessages,parseMbox)
import qualified Data.ByteString.Lazy as B
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do hPutStrLn stderr "Reading from stdin..."
          (print . length . mboxMessages . parseMbox) =<< B.getContents
