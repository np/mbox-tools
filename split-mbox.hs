{-# LANGUAGE BangPatterns, ExistentialQuantification, TemplateHaskell,
             TypeOperators #-}
--------------------------------------------------------------------
-- |
-- Executable : split-mbox
-- Copyright : (c) Nicolas Pouillard 2008, 2009, 2011
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

import Control.Arrow
import Codec.Mbox (Mbox(..),MboxMessage,Direction(..),msgYear,msgMonthYear,parseMboxFile,showMbox)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Label
import Data.Char (toLower)
import Data.List (groupBy)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO (hFlush, stdout)

equating :: Eq b => (a -> b) -> a -> a -> Bool
equating f x y = f x == f y

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = uncurry (:) $ second (chunks n) $ splitAt n xs

-- Year is first
data SplitBy = Year | Month
  deriving (Show, Enum)

data Settings = Settings { _help :: Bool, _splitBy :: SplitBy }
$(mkLabels [''Settings])
type Flag = Settings -> Settings

splitMbox :: Eq a => (MboxMessage C.ByteString -> a) -> (MboxMessage C.ByteString -> String) -> String -> IO ()
splitMbox keyMsg fmtMsg mboxfile = do
  -- this 'chunks' trick is to avoid a lazyness problem
  mapM_ go . concatMap (groupBy (equating keyMsg)) . chunks 1000 . mboxMessages =<< parseMboxFile Forward mboxfile
  putStrLn "done."
  where go ms = do let !fp = "mbox-" ++ (fmtMsg $ head ms)
                   putStr ("\rWriting to " ++ fp ++ "...")
                   hFlush stdout
                   C.appendFile fp . showMbox . Mbox $ ms

splitMboxWith :: Settings -> String -> IO ()
splitMboxWith settings =
  case get splitBy settings of
    Year  -> splitMbox msgYear (show . msgYear)
    Month -> splitMbox msgMonthYear (uncurry (++) . (map toLower . show *** ('-':) . show) . msgMonthYear)

defaultSettings :: Settings
defaultSettings = Settings { _help = False, _splitBy = Year }

usage :: String -> a
usage msg = error (msg ++ "\n" ++ usageInfo header options)
  where header = "Usage: split-mbox [OPTION...] mbox-files..."

options :: [OptDescr Flag]
options =
  [ Option "?" ["help"] (NoArg (set help True)) "Show this help message"
  , byOpt ]

byOpt :: OptDescr Flag
byOpt = Option "b" ["by"] (ReqArg (set splitBy . parseBy) "ARG") desc
  where parseBy = fromMaybe (usage "Bad argument") . (`lookup` args)
        args = map ((map toLower . show) &&& id) [ Year .. ]
        desc = "Split by " ++ show (map fst args)

main :: IO ()
main = do
  args <- getArgs
  let (flags, nonopts, errs) = getOpt Permute options args
  let opts = foldr ($) defaultSettings flags
  if get help opts
   then usage ""
   else
    case (nonopts, errs) of
      ([],        []) -> usage "Too few arguments"
      (mboxfiles, []) -> mapM_ (splitMboxWith opts) mboxfiles
      (_,          _) -> usage (concat errs)

