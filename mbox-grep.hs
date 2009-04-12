{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------
-- |
-- Executable : mbox-grep
-- Copyright : (c) Nicolas Pouillard 2008, 2009
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

import Codec.Mbox (Mbox(..), MboxMessage(..), Direction(..), parseMboxFiles, opposite)
import Email (Email(..),ShowFormat(..),fmtOpt,defaultShowFormat,
              readEmail,putEmails,showFormatsDoc,stringOfField)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (parse)
import Hutt.Query (evalQueryMsg,parseQuery)
import Hutt.Types(Msg(..),Dsc(..),MsgId(..),DscId(..),Query(..))
import Data.Tree (Tree(..))
import Data.Accessor
import Data.Accessor.Template
import Control.Arrow
import System.Console.GetOpt

grepMbox :: Settings -> String -> [String] -> IO ()
grepMbox opts queryString = (mapM_ f =<<) . parseMboxFiles (dir opts)
  where query = either (error "malformed query") id $ parse parseQuery "<first-argument>" queryString
        f     = putEmails (fmt opts)
                . filter (emailMatchQuery query . fst)
                . map ((readEmail . mboxMsgBody) &&& id) . mboxMessages

emailMatchQuery :: Query -> Email -> Bool
emailMatchQuery query email = evalQueryMsg (msg, dsc) query
  where msg = Msg { msgHeader = map stringOfField $ emailFields email
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

data Settings = Settings { fmt  :: ShowFormat
                         , dir  :: Direction
                         , help :: Bool
                         }
$(nameDeriveAccessors ''Settings $ Just.(++ "A"))
type Flag = Settings -> Settings

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

defaultSettings :: Settings
defaultSettings = Settings { fmt  = defaultShowFormat
                           , dir  = Forward
                           , help = False
                           }

usage :: String -> a
usage msg = error $ unlines [msg, usageInfo header options, showFormatsDoc]
  where header = "Usage: mbox-grep [OPTIONS] <query> <mbox-file>*"

options :: [OptDescr Flag]
options =
  [ fmtOpt usage (fmtA ^=)
  , Option "r" ["reverse"] (NoArg (dirA ^: opposite)) "Reverse the mbox order (latest firsts)"
  , Option "?" ["help"]    (NoArg (helpA ^= True)) "Show this help message"
  ]

