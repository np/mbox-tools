--------------------------------------------------------------------
-- |
-- Executable : mbox-pick
-- Copyright : (c) Nicolas Pouillard 2008
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

import Control.Arrow
import Control.Monad
import Control.Applicative
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.ByteString.Lazy.Char8 as C
import Codec.Mbox (MboxMessage(..),Direction(..),parseOneMboxMessage)
import Email (readEmail,putEmails,ShowFormat(..),fmtOpt,defaultShowFormat,showFormatsDoc)
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO (FilePath, IOMode(..), stdin, openFile, hClose)

mayRead :: Read a => String -> Maybe a
mayRead s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing

parseSeq :: C.ByteString -> Maybe [Integer]
parseSeq = mapM (mayRead . C.unpack) . C.split ','

pickMbox :: Settings -> String -> Maybe FilePath -> IO ()
pickMbox opts sequ' mmbox = do
  mbox <- maybe (return stdin) (`openFile` ReadMode) mmbox
  sequ <- maybe (fail "badly formatted comma separated offset sequence") return $ parseSeq $ C.pack sequ'
  mails <- mapM ((((readEmail . mboxMsgBody) &&& id) <$>) . parseOneMboxMessage (fromMaybe "" mmbox) mbox) sequ
  putEmails (fmt opts) mails
  hClose mbox

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
  where header = "Usage: mbox-pick [OPTION] <offset0>,<offset1>,...,<offsetN> <mbox-file>"

options :: [OptDescr Flag]
options =
  [ fmtOpt usage setFmt
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
      ([],          []) -> usage "Too few arguments"
      (_:_:_:_,     []) -> usage "Too many arguments"
      (sequ : mbox, []) -> pickMbox opts sequ $ listToMaybe mbox
      (_,            _) -> usage (concat errs)

