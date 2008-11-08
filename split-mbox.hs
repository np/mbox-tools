{-# LANGUAGE BangPatterns, ExistentialQuantification #-}
--------------------------------------------------------------------
-- |
-- Executable : split-mbox
-- Copyright : (c) Nicolas Pouillard 2008
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

import Control.Arrow
import Mbox (Mbox(..),MboxMessage,mboxMsgTime)
import Mbox.ByteString.Lazy (Direction(..),parseMboxFile,printMbox)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (toLower)
import Data.List (groupBy)
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO (hFlush, stdout)

equating :: Eq b => (a -> b) -> a -> a -> Bool
equating f x y = f x == f y

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = uncurry (:) $ second (chunks n) $ splitAt n xs

type Msg = MboxMessage C.ByteString

readYear :: Msg -> C.ByteString -> Int
readYear m s =
 case reads $ C.unpack s of
   [(i, "")] -> i
   _         -> error ("readYear: badly formatted date (year) in " ++ show (C.unpack $ mboxMsgTime m))

data Month = Jan
           | Feb
           | Mar
           | Apr
           | May
           | Jun
           | Jul
           | Aug
           | Sep
           | Oct
           | Nov
           | Dec
  deriving (Read, Show, Eq)

readMonth :: Msg -> C.ByteString -> Month
readMonth m s =
  case reads $ C.unpack s of
    [(month, "")] -> month
    _             -> error ("readMonth: badly formatted date (month) in " ++ show (C.unpack $ mboxMsgTime m))

msgYear :: Msg -> Int
msgYear m = readYear m . last . C.split ' ' . mboxMsgTime $ m

msgMonthYear :: Msg -> (Month,Int)
msgMonthYear m =
 case filter (not . C.null) . C.split ' ' . mboxMsgTime $ m of
   [_wday, month, _mday, _hour, year] -> (readMonth m month, readYear m year)
   _ -> error ("msgMonthYear: badly formatted date in " ++ show (C.unpack $ mboxMsgTime m))

splitMbox :: Eq a => (Msg -> a) -> (Msg -> String) -> String -> IO ()
splitMbox keyMsg fmtMsg mboxfile =
  -- this 'chunks' trick is to avoid a lazyness problem
  mapM_ go . concatMap (groupBy (equating keyMsg)) . chunks 1000 . unMbox =<< parseMboxFile Forward mboxfile
  where go ms = do let !fp = "mbox-" ++ (fmtMsg $ head ms)
                   putStr ("\rWriting to " ++ fp ++ "...")
                   hFlush stdout
                   C.appendFile fp . printMbox . Mbox $ ms

splitMboxWith :: Settings -> String -> IO ()
splitMboxWith settings =
  case splitBy settings of
    Year  -> splitMbox msgYear (show . msgYear)
    Month -> splitMbox msgMonthYear (uncurry (++) . (map toLower . show *** ('-':) . show) . msgMonthYear)

data Settings = Settings { help :: Bool, splitBy :: SplitBy }
type Flag = Settings -> Settings

-- Year is first
data SplitBy = Year | Month
  deriving (Show, Enum)

defaultSettings :: Settings
defaultSettings = Settings { help = False, splitBy = Year }

setHelp :: Settings -> Settings
setHelp s = s { help = True }

setSplitBy :: SplitBy -> Settings -> Settings
setSplitBy x s = s { splitBy = x }

usage :: String -> a
usage msg = error (msg ++ "\n" ++ usageInfo header options)
  where header = "Usage: split-mbox [OPTION...] mbox-files..."

options :: [OptDescr Flag]
options =
  [ Option ['?'] ["help"] (NoArg setHelp) "Show this help message"
  , byOpt ]

byOpt :: OptDescr Flag
byOpt = Option ['b'] ["by"] (ReqArg (setSplitBy . parseBy) "ARG") desc
  where parseBy = maybe (usage "Bad argument") id . (`lookup` args)
        args = map ((map toLower . show) &&& id) [ Year .. ]
        desc = "Split by " ++ show (map fst args)

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
      (mboxfiles, []) -> mapM_ (splitMboxWith opts) mboxfiles
      (_,          _) -> usage (concat errs)

