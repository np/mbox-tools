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
import Mbox.ByteString.Lazy (Direction(..),parseMboxFile,printMbox)
import Email (readEmail,showEmailAsOneLinerDebug,ShowFormat(..),fmtOpt)
import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs)
import System.Console.GetOpt

listMbox :: Settings -> String -> IO ()
listMbox opts mboxfile = do
  mbox <- parseMboxFile (dir opts) mboxfile
  case fmt opts of
    MboxFmt       -> B.putStr $ printMbox mbox
    OneLinerDebug -> mapM_ (putStrLn . showEmailAsOneLinerDebug . readEmail . mboxMsgBody) . unMbox $ mbox

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

