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

import Mbox (Mbox(..), MboxMessage(..), Direction(..), parseMboxFile)
import Email (Email(..),ShowFormat(..),fmtOpt,readEmail,putEmails)
import System.Environment (getArgs)
import Text.Parsec (parse)
import Hutt.Query (evalQueryMsg,parseQuery)
import Hutt.Types(GenMsg(..),GenDsc(..),MsgId(..),DscId(..),Query(..))
import Data.Tree (Tree(..))
import Control.Arrow
import System.Console.GetOpt

grepMbox :: Settings -> String -> String -> IO ()
grepMbox opts mbox queryString = do
  let query = either (error "malformed query") id $ parse parseQuery "<first-argument>" queryString
  input <- parseMboxFile (dir opts) mbox
  putEmails (fmt opts)
    $ filter (emailMatchQuery query . fst)
    $ map ((readEmail . mboxMsgBody) &&& id) . unMbox
    $ input

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
  args <- getArgs
  let (flags, nonopts, errs) = getOpt Permute options args
  let opts = foldr ($) defaultSettings flags
  if help opts
   then usage ""
   else
    case (nonopts, errs) of
      ([],                  []) -> usage "Too few arguments"
      ([mbox, queryString], []) -> grepMbox opts mbox queryString
      (_,                   []) -> usage "Too many arguments"
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
defaultSettings = Settings { fmt  = OneLinerDebug
                           , dir  = Forward
                           , help = False
                           }

setFmt :: ShowFormat -> Settings -> Settings
setFmt f s = s { fmt = f }

setHelp :: Settings -> Settings
setHelp s = s { help = True }

usage :: String -> a
usage msg = error (msg ++ "\n" ++ usageInfo header options)
  where header = "Usage: mbox-grep [OPTION...] <mbox-file> <query>"

options :: [OptDescr Flag]
options =
  [ fmtOpt usage setFmt
  , Option ['r'] ["reverse"] (NoArg flipDir) "Reverse the mbox order (latest firsts)"
  , Option ['?'] ["help"]    (NoArg setHelp) "Show this help message"
  ]

