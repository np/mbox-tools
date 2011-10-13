{-# LANGUAGE TemplateHaskell, TypeOperators #-}
--------------------------------------------------------------------
-- |
-- Executable : mbox-pick
-- Copyright : (c) Nicolas Pouillard 2008, 2009, 2011
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

import Control.Arrow
import Control.Applicative
import Data.Label
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.ByteString.Lazy.Char8 as C
import Codec.Mbox (mboxMsgBody,parseOneMboxMessage)
import Email (readEmail)
import EmailFmt (putEmails,ShowFormat(..),fmtOpt,defaultShowFormat,showFormatsDoc)
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO (IOMode(..), stdin, openFile, hClose)

mayRead :: Read a => String -> Maybe a
mayRead s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing

parseSeq :: C.ByteString -> Maybe [Integer]
parseSeq = mapM (mayRead . C.unpack) . C.split ','

data Settings = Settings { _fmt  :: ShowFormat
                         , _help :: Bool
                         }
$(mkLabels [''Settings])

type Flag = Settings -> Settings

pickMbox :: Settings -> String -> Maybe FilePath -> IO ()
pickMbox opts sequ' mmbox = do
  mbox <- maybe (return stdin) (`openFile` ReadMode) mmbox
  sequ <- maybe (fail "badly formatted comma separated offset sequence") return $ parseSeq $ C.pack sequ'
  mails <- mapM ((((readEmail . get mboxMsgBody) &&& id) <$>) . parseOneMboxMessage (fromMaybe "" mmbox) mbox) sequ
  putEmails (get fmt opts) mails
  hClose mbox

defaultSettings :: Settings
defaultSettings = Settings { _fmt  = defaultShowFormat
                           , _help = False
                           }

usage :: String -> a
usage msg = error $ unlines [msg, usageInfo header options, showFormatsDoc]
  where header = "Usage: mbox-pick [OPTION] <offset0>,<offset1>,...,<offsetN> <mbox-file>"

options :: [OptDescr Flag]
options =
  [ fmtOpt usage (set fmt)
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
      ([],          []) -> usage "Too few arguments"
      (_:_:_:_,     []) -> usage "Too many arguments"
      (sequ : mbox, []) -> pickMbox opts sequ $ listToMaybe mbox
      (_,            _) -> usage (concat errs)

