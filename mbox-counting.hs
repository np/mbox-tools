import Mbox (unMbox)
import Control.Applicative ((<$>))
import Mbox.ByteString.Lazy (parseMboxFile,Direction(..))
import qualified Data.ByteString.Lazy as B
import System.Environment
import Data.List (delete)

main :: IO ()
--main = print =<< (mboxCountMessages . parseMbox) <$> B.getContents
main = do
  args <- getArgs
  let (dir,args') = if "-r" `elem` args then (Backward,delete "-r" args) else (Forward,args)
  print =<< (length . unMbox) <$> parseMboxFile dir (head args')
