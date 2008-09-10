--------------------------------------------------------------------
-- |
-- Executable : mbox-grep
-- Copyright : (c) Nicolas Pouillard 2008
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

import Mbox (unMbox, mboxMsgBody)
import Mbox.ByteString.Lazy (parseMbox)
import Email (Email(..),readEmail,showEmailAsOneLinerDebug)
import qualified Data.ByteString.Lazy as B (readFile)
import System.Environment (getArgs)
import Text.Parsec (parse)
import Hutt.Query (evalQueryMsg,parseQuery)
import Hutt.Types(GenMsg(..),GenDsc(..),MsgId(..),DscId(..),Query(..))
import Data.Tree (Tree(..))

emailMatchQuery :: Query -> Email -> Bool
emailMatchQuery query email = evalQueryMsg dsc msg query
  where msg = Msg { msgHeader = []
                  , msgBody = rawEmail email
                  , msgContent = emailContent email
                  , msgId = MsgId 0
                  , msgLabels = []
                  , msgParents = []
                  , msgReferences = [] }
        dsc = Dsc { dscId = DscId 0
                  , dscMsgs = Node msg []
                  , dscLabels = [] }

main :: IO ()
main = do
  [queryString,mbox] <- getArgs
  let query = either (error "malformed query") id $ parse parseQuery "<first-argument>" queryString
  input <- B.readFile mbox
  mapM_ (putStrLn . showEmailAsOneLinerDebug)
    $ filter (emailMatchQuery query)
    $ map (readEmail . mboxMsgBody) . unMbox . parseMbox
    $ input
