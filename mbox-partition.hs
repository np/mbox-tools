{-# LANGUAGE TemplateHaskell, TypeOperators #-}
--------------------------------------------------------------------
-- |
-- Executable : mbox-partition
-- Copyright : (c) Nicolas Pouillard 2008, 2009, 2011
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

import Control.Applicative
import Codec.Mbox (Mbox(..),Direction(..),parseMboxFile,mboxMsgBody,showMboxMessage)
import Email (Email(..),emailFields,readEmail)
import Text.ParserCombinators.Parsec.Rfc2822 (Field(MessageID))
import Data.Label
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Set (fromList, member)
import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO (Handle, IOMode(AppendMode), stderr, openFile, hPutStr, hFlush, hClose)

progressStr :: String -> IO ()
progressStr s = hPutStr stderr ('\r':s) >> hFlush stderr

progress_ :: [IO a] -> IO ()
progress_ = (>> progressStr "Finished\n") . sequence_ . zipWith (>>) (map (progressStr . show) [(1::Int)..])

-- should be in ByteString
hPutStrLnC :: Handle -> C.ByteString -> IO ()
hPutStrLnC h s = C.hPut h s >> C.hPut h (C.pack "\n")

data Settings = Settings { _help :: Bool
                         , _msgids :: String
                         , _inside :: String
                         , _outside :: String
                         }
$(mkLabels [''Settings])
type Flag = Settings -> Settings

partitionMbox :: Settings -> [String] -> IO ()
partitionMbox opts mboxfiles = do
  msgids' <- (fromList . C.lines) <$> C.readFile (get msgids opts)
  let predicate = fromMaybe False . fmap (`member` msgids') . emailMsgId . readEmail . get mboxMsgBody
  hinside <- openFile (get inside opts) AppendMode
  houtside <- openFile (get outside opts) AppendMode
  let onFile fp =
        progress_ . map (\m -> hPutStrLnC (if predicate m then hinside else houtside) (showMboxMessage m))
                  . mboxMessages
           =<< parseMboxFile Forward fp
  mapM_ onFile mboxfiles
  mapM_ hClose [hinside, houtside]

emailMsgId :: Email -> Maybe C.ByteString
emailMsgId m = listToMaybe [ removeAngles $ C.pack i | MessageID i <- get emailFields m ]

removeAngles :: C.ByteString -> C.ByteString
removeAngles = C.takeWhile (/='>') . C.dropWhile (=='<')

defaultSettings :: Settings
defaultSettings = Settings { _help = False
                           , _msgids = ""
                           , _inside = ""
                           , _outside = "" }

usage :: String -> a
usage msg = error (msg ++ "\n" ++ usageInfo header options)
  where header = "Usage: mbox-partition [OPTION...] <mbox-file>*"

options :: [OptDescr Flag]
options =
  [ Option "m" ["msgids"]  (ReqArg (set msgids) "FILE") "A file with message-IDs"
  , Option "i" ["inside"]  (ReqArg (set inside) "FILE") "Will receive messages referenced by the 'msgids' file"
  , Option "o" ["outside"] (ReqArg (set outside) "FILE") "Will receive messages *NOT* referenced by the 'msgids' file"
  , Option "?" ["help"]    (NoArg  (set help True)) "Show this help message"
  ]

main :: IO ()
main = do
  args <- getArgs
  let (flags, nonopts, errs) = getOpt Permute options args
  let opts = foldr ($) defaultSettings flags
  case (nonopts, errs) of
    _ | get help opts -> usage ""
    (_, _:_)          -> usage (concat errs)
    ([], _)           -> usage "Too few arguments (mbox-file missing)"
    (mboxfiles, _)    -> partitionMbox opts mboxfiles

