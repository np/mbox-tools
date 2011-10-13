{-# LANGUAGE TemplateHaskell, TypeOperators #-}
--------------------------------------------------------------------
-- |
-- Executable : mbox-list
-- Copyright : (c) Nicolas Pouillard 2008, 2009, 2011
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

import Prelude
import Control.Arrow
import Codec.Mbox (Mbox(..),Direction(..),parseMboxFiles,mboxMsgBody,opposite)
import Email (readEmail)
import EmailFmt (putEmails,ShowFormat(..),fmtOpt,defaultShowFormat,showFormatsDoc)
import System.Environment (getArgs)
import System.Console.GetOpt
import Data.Label

data Settings = Settings { _fmt      :: ShowFormat
                         , _dir      :: Direction
                         , _takeOpt  :: Maybe Int
                         , _dropOpt  :: Maybe Int
                         , _help     :: Bool
                         }
$(mkLabels [''Settings])

type Flag = Settings -> Settings

listMbox :: Settings -> [String] -> IO ()
listMbox opts mboxfiles =
  mapM_ (putEmails (get fmt opts) .
         map ((readEmail . get mboxMsgBody) &&& id) .
         maybe id take (get takeOpt opts) .
         maybe id drop (get dropOpt opts) .
         mboxMessages)
    =<< parseMboxFiles (get dir opts) mboxfiles

defaultSettings :: Settings
defaultSettings = Settings { _fmt      = defaultShowFormat
                           , _dir      = Forward
                           , _takeOpt  = Nothing
                           , _dropOpt  = Nothing
                           , _help     = False
                           }

usage :: String -> a
usage msg = error $ unlines [msg, usageInfo header options, showFormatsDoc]
  where header = "Usage: mbox-list [OPTION] <mbox-file>*"

maybeIntArg :: (Settings :-> Maybe Int) -> ArgDescr (Settings -> Settings)
maybeIntArg l = ReqArg (set l . Just . read) "NUM"

-- Since
--   ∀ k1 k2 Positives, take k1 . drop k2 == drop k2 . take (k2 + k1)
-- one fix an ordering: drop then take.
options :: [OptDescr Flag]
options =
  [ fmtOpt usage (set fmt)
  , Option "r" ["reverse"]  (NoArg (modify dir opposite)) "Reverse the mbox order (latest firsts)"
  , Option "d" ["drop"]     (maybeIntArg dropOpt)         "Drop the NUM firsts"
  , Option "t" ["take"]     (maybeIntArg takeOpt)         "Take the NUM firsts (happens after --drop)"
  , Option "?" ["help"]     (NoArg (set help True))       "Show this help message"
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

