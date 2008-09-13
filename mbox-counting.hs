import Mbox (unMbox)
import Mbox.ByteString.Lazy (parseMbox)
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = (print . length . unMbox . parseMbox) =<< B.getContents
