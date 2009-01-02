{-# LANGUAGE TemplateHaskell #-}
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

import Control.Arrow
import Codec.Mbox (Mbox(..),Direction(..),parseMboxFiles,mboxMsgBody,opposite)
import Email (readEmail,putEmails,ShowFormat(..),fmtOpt,defaultShowFormat,showFormatsDoc)
import System.Environment (getArgs)
import System.Console.GetOpt
import Data.Accessor
import Data.Accessor.Template

listMbox :: Settings -> [String] -> IO ()
listMbox opts mboxfiles =
  mapM_ (putEmails (fmt opts) . map ((readEmail . mboxMsgBody) &&& id) . unMbox)
    =<< parseMboxFiles (dir opts) mboxfiles

data Settings = Settings { fmt  :: ShowFormat
                         , dir  :: Direction
                         , help :: Bool
                         }
$(nameDeriveAccessors ''Settings $ Just.(++ "A"))

type Flag = Settings -> Settings

defaultSettings :: Settings
defaultSettings = Settings { fmt  = defaultShowFormat
                           , dir  = Forward
                           , help = False
                           }

usage :: String -> a
usage msg = error $ unlines [msg, usageInfo header options, showFormatsDoc]
  where header = "Usage: mbox-list [OPTION] <mbox-file>*"

options :: [OptDescr Flag]
options =
  [ fmtOpt usage (fmtA ^=)
  , Option "r" ["reverse"] (NoArg (dirA ^: opposite)) "Reverse the mbox order (latest firsts)"
  , Option "?" ["help"]    (NoArg (helpA ^= True)) "Show this help message"
  ]

main :: IO ()
main = do
  args <- getArgs
  let (flags, nonopts, errs) = getOpt Permute options args
  let opts = foldr ($) defaultSettings flags
  if help opts
   then usage ""
   else
    case (nonopts, errs) of
      (mboxfiles, []) -> listMbox opts mboxfiles
      (_,          _) -> usage (concat errs)

