{-# LANGUAGE TemplateHaskell, TypeOperators #-}
--------------------------------------------------------------------
-- |
-- Executable : mbox-list
-- Copyright : (c) Nicolas Pouillard 2008, 2009
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

import Prelude hiding (mod)
import Control.Arrow
import Codec.Mbox (Mbox(..),Direction(..),parseMboxFiles,mboxMsgBody,opposite)
import Email (readEmail)
import EmailFmt (putEmails,ShowFormat(..),fmtOpt,defaultShowFormat,showFormatsDoc)
import System.Environment (getArgs)
import System.Console.GetOpt
import Data.Record.Label

data Settings = Settings { _fmt  :: ShowFormat
                         , _dir  :: Direction
                         , _help :: Bool
                         }
$(mkLabels [''Settings])
help :: Settings :-> Bool
dir  :: Settings :-> Direction
fmt  :: Settings :-> ShowFormat


type Flag = Settings -> Settings

listMbox :: Settings -> [String] -> IO ()
listMbox opts mboxfiles =
  mapM_ (putEmails (get fmt opts) . map ((readEmail . mboxMsgBody) &&& id) . mboxMessages)
    =<< parseMboxFiles (get dir opts) mboxfiles

defaultSettings :: Settings
defaultSettings = Settings { _fmt  = defaultShowFormat
                           , _dir  = Forward
                           , _help = False
                           }

usage :: String -> a
usage msg = error $ unlines [msg, usageInfo header options, showFormatsDoc]
  where header = "Usage: mbox-list [OPTION] <mbox-file>*"

options :: [OptDescr Flag]
options =
  [ fmtOpt usage (set fmt)
  , Option "r" ["reverse"] (NoArg (mod dir opposite)) "Reverse the mbox order (latest firsts)"
  , Option "?" ["help"]    (NoArg (set help True)) "Show this help message"
  ]

main :: IO ()
main = do
  args <- getArgs
  let (flags, nonopts, errs) = getOpt Permute options args
  let opts = foldr ($) defaultSettings flags
  if get help opts
   then usage ""
   else
    case (nonopts, errs) of
      (mboxfiles, []) -> listMbox opts mboxfiles
      (_,          _) -> usage (concat errs)

