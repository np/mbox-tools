--------------------------------------------------------------------
-- |
-- Executable : mbox-list
-- Copyright : (c) Nicolas Pouillard 2008
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

import Mbox (unMbox,mboxMsgBody)
import Mbox.ByteString.Lazy (parseMbox)
import Email (readEmail,showEmail,ShowFormat(..))
import qualified Data.ByteString.Lazy as B (readFile)
import System.Environment (getArgs)

main :: IO ()
main = do
  [arg] <- getArgs
  let fmt = OneLinerDebug
  mapM_ (putStrLn . showEmail fmt . readEmail . mboxMsgBody) . unMbox . parseMbox =<< B.readFile arg

