import Mbox (Mbox(..), MboxMessage(..), mboxCountMessages)
import Control.Applicative ((<$>))
import Mbox.ByteString.Lazy (parseMbox)
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = print =<< (mboxCountMessages . parseMbox) <$> B.getContents
