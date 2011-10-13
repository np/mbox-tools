{-# LANGUAGE TemplateHaskell, TypeOperators, ScopedTypeVariables #-}
--------------------------------------------------------------------
-- |
-- Executable : mbox-iter
-- Copyright : (c) Nicolas Pouillard 2009, 2011
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

-- import Control.Arrow
import Control.Exception
import Codec.Mbox (Mbox(..),Direction(..),parseMboxFiles,opposite,showMboxMessage)
import System.Environment (getArgs)
import System.Console.GetOpt
import System.Exit
import System.IO
import qualified System.Process as P
import qualified Data.ByteString.Lazy as L
import Data.Label

data Settings = Settings { _dir  :: Direction
                         , _help :: Bool
                         }
$(mkLabels [''Settings])

type Flag = Settings -> Settings

systemWithStdin :: String -> L.ByteString -> IO ExitCode
systemWithStdin shellCmd input = do
  (Just stdinHdl, _, _, pHdl) <-
     P.createProcess (P.shell shellCmd){ P.std_in = P.CreatePipe }
  handle (\(_ :: IOException) -> return ()) $ do
    L.hPut stdinHdl input
    hClose stdinHdl
  P.waitForProcess pHdl

iterMbox :: Settings -> String -> [String] -> IO ()
iterMbox opts cmd mboxfiles =
  mapM_ (mapM_ (systemWithStdin cmd . showMboxMessage) . mboxMessages)
    =<< parseMboxFiles (get dir opts) mboxfiles

defaultSettings :: Settings
defaultSettings = Settings { _dir  = Forward
                           , _help = False
                           }

usage :: String -> a
usage msg = error $ unlines [msg, usageInfo header options]
  where header = "Usage: mbox-iter [OPTION] <cmd> <mbox-file>*"

options :: [OptDescr Flag]
options =
  [ Option "r" ["reverse"] (NoArg (modify dir opposite)) "Reverse the mbox order (latest firsts)"
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
      (cmd : mboxfiles, []) -> iterMbox opts cmd mboxfiles
      (_,                _) -> usage (concat errs)

