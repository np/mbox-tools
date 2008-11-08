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

import Control.Arrow
import Mbox (Mbox(..),Direction(..),parseMboxFile,mboxMsgBody)
import Email (readEmail,putEmails,ShowFormat(..),fmtOpt)
import System.Environment (getArgs)
import System.Console.GetOpt

listMbox :: Settings -> String -> IO ()
listMbox opts mboxfile =
  putEmails (fmt opts) . map ((readEmail . mboxMsgBody) &&& id) . unMbox =<< parseMboxFile (dir opts) mboxfile

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
  where header = "Usage: mbox-list [OPTION...] mbox-files..."

options :: [OptDescr Flag]
options =
  [ fmtOpt usage setFmt
  , Option ['r'] ["reverse"] (NoArg flipDir) "Reverse the mbox order (latest firsts)"
  , Option ['?'] ["help"]    (NoArg setHelp) "Show this help message"
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
      ([],        []) -> usage "Too few arguments"
      (mboxfiles, []) -> mapM_ (listMbox opts) mboxfiles
      (_,          _) -> usage (concat errs)

