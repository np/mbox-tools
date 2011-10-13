{-# LANGUAGE TemplateHaskell, TypeOperators #-}
--------------------------------------------------------------------
-- |
-- Executable : mbox-from-files
-- Copyright : (c) Nicolas Pouillard 2010, 2011
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

import Prelude hiding (mod)
import Control.Monad (join)
import Codec.Mbox (Mbox(..),MboxMessage(..),showMbox)
import System.Environment (getArgs)
import System.Console.GetOpt
import Data.Label
import qualified Data.ByteString.Lazy.Char8 as B

data Settings = Settings { _help :: Bool }

$(mkLabels [''Settings])

type Flag = Settings -> Settings

msgFromFile :: FilePath -> IO (MboxMessage B.ByteString)
msgFromFile fp =
 do contents <- B.readFile fp
    return $ MboxMessage { _mboxMsgSender = B.pack "XXX"
                         , _mboxMsgTime   = B.pack "Thu Jan 01 01:00:00 +0100 1970"
                         , _mboxMsgBody   = contents
                         , _mboxMsgFile   = fp
                         , _mboxMsgOffset = undefined }

mboxFromFiles :: Settings -> [FilePath] -> IO ()
mboxFromFiles _ =
  join . fmap (B.putStr . showMbox . Mbox)
       . mapM msgFromFile

defaultSettings :: Settings
defaultSettings = Settings { _help = False }

usage :: String -> a
usage msg = error $ unlines ls
  where header = "Usage: mbox-from-files [OPTION] <mail-files>*"
        ls     = msg : usageInfo header options :
                 ["If no files are given as argument, a list of"
                  ,"file names is read from the standard input."]

options :: [OptDescr Flag]
options =
  [ Option "?" ["help"]     (NoArg (set help True))     "Show this help message"
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
      ([], [])    -> mboxFromFiles opts . lines =<< getContents
      (files, []) -> mboxFromFiles opts files
      (_,      _) -> usage (concat errs)

