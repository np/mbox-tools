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

import Mbox (Mbox(..), MboxMessage(..), Direction(..), parseMboxFiles)
import Email (Email(..),ShowFormat(..),fmtOpt,defaultShowFormat,readEmail,putEmails,showFormatsDoc)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (parse)
import Hutt.Query (evalQueryMsg,parseQuery)
import Hutt.Types(Msg(..),Dsc(..),MsgId(..),DscId(..),Query(..))
import Data.Tree (Tree(..))
import Control.Arrow
import System.Console.GetOpt

grepMbox :: Settings -> String -> [String] -> IO ()
grepMbox opts queryString = (mapM_ f =<<) . parseMboxFiles (dir opts)
  where query = either (error "malformed query") id $ parse parseQuery "<first-argument>" queryString
        f =
          putEmails (fmt opts)
            . filter (emailMatchQuery query . fst)
            . (map ((readEmail . mboxMsgBody) &&& id) . unMbox)

emailMatchQuery :: Query -> Email -> Bool
emailMatchQuery query email = evalQueryMsg id dsc msg query
  where msg = Msg { msgHeader = []
                  --, msgBody = rawEmail email
                  , msgContent = rawEmail email -- emailContent email
                  , msgId = MsgId 0
                  , msgLabels = []
                  , msgAddress = undefined
                  , msgParent = Nothing
                  , msgReferences = [] }
        dsc = Dsc { dscId = DscId 0
                  , dscMsgs = Node () []
                  , dscLabels = [] }

main :: IO ()
main = do
  args <- getArgs
  let (flags, nonopts, errs) = getOpt Permute options args
  let opts = foldr ($) defaultSettings flags
  if help opts
   then usage ""
   else
    case (nonopts, errs) of
      (queryString : files, []) -> grepMbox opts queryString files
      (_,                   []) -> usage "Too few arguments"
      _                         -> usage (concat errs)

flipDir :: Settings -> Settings
flipDir s = s { dir = opposite $ dir s }
  where opposite Backward = Forward
        opposite Forward  = Backward

data Settings = Settings { fmt  :: ShowFormat
                         , dir  :: Direction
                         , help :: Bool
                         }
type Flag = Settings -> Settings

defaultSettings :: Settings
defaultSettings = Settings { fmt  = defaultShowFormat
                           , dir  = Forward
                           , help = False
                           }

setFmt :: ShowFormat -> Settings -> Settings
setFmt f s = s { fmt = f }

setHelp :: Settings -> Settings
setHelp s = s { help = True }

usage :: String -> a
usage msg = error $ unlines [msg, usageInfo header options, showFormatsDoc]
  where header = "Usage: mbox-grep [OPTIONS] <query> <mbox-file>*"

options :: [OptDescr Flag]
options =
  [ fmtOpt usage setFmt
  , Option ['r'] ["reverse"] (NoArg flipDir) "Reverse the mbox order (latest firsts)"
  , Option ['?'] ["help"]    (NoArg setHelp) "Show this help message"
  ]

