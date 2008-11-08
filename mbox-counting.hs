import Mbox (unMbox,parseMbox)
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do putStrLn "Reading from stdin..."
          (print . length . unMbox . parseMbox) =<< B.getContents
