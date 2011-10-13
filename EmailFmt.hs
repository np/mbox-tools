--------------------------------------------------------------------
-- |
-- Module    : EmailFmt
-- Copyright : (c) Nicolas Pouillard 2010, 2011
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

{-# LANGUAGE Rank2Types,
             OverloadedStrings, GeneralizedNewtypeDeriving #-}
module EmailFmt where

import Control.Applicative
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Internal as B
import System.Console.GetOpt (OptDescr(..),ArgDescr(..))
import System.IO (Handle, stdout, hPutChar)
import Codec.Mbox (Mbox(..), MboxMessage(..), showMbox)
import Data.Maybe (fromMaybe)
import Email
import FmtComb

data ShowFormat = MboxFmt
                | FmtComb FmtComb

fmtOpt :: (forall err. String -> err) -> (ShowFormat -> a) -> OptDescr a
fmtOpt usage f = Option "f" ["fmt"] (ReqArg (f . parseFmt) "FMT") desc
  where parseFmt = fromMaybe (usage "Bad display format") . mayReadShowFormat
        desc = "Choose the display format"

defaultShowFormat :: ShowFormat
defaultShowFormat = FmtComb oneLinerF

mayReadShowFormat :: String -> Maybe ShowFormat
mayReadShowFormat "mbox" = Just MboxFmt
mayReadShowFormat xs     = FmtComb <$> mayReadShowFmts xs

showFormatsDoc :: String
showFormatsDoc = unlines $
       ["Message formatting:"
       ,""
       ,"  fmt  ::= 'mbox'"
       ,"         | ( '%(' (<fct> '.')* <name> ')' | <string> )*"
       ,"  name ::="] ++
  map (("         | '" ++) . (++ "'") . fst) fmtCombs ++
       ["  fct  ::="] ++
  map (("         | '" ++) . (++ "'") . fst) fmtMods ++
  map (("         | '" ++) . (++ "' <int>") . fst) intFmtMods ++
       [""
       ,"  * one : One line per email with: subject, mimetype and message ID (default)"
       ,"  * mbox: Write emails in mbox format"
       ,"  * from: One line header of mbox format [as 'From %(mboxmsgsender) %(mboxmsgtime)']"
       ] ++
  map (\ (x, (_, y)) -> "  * " ++ x ++ ": " ++ y) fmtMods ++
  map (\ (x, (_, y)) -> "  * " ++ x ++ ": " ++ y) intFmtMods

hPutB' :: Handle -> B.ByteString -> IO ()
hPutB' h = go
  where go B.Empty        = return ()
        go (B.Chunk c cs) = S.hPut h c >> go cs

putStrLnB' :: B.ByteString -> IO ()
putStrLnB' s = hPutB' stdout s >> hPutChar stdout '\n'

putEmails :: ShowFormat -> [(Email,MboxMessage B.ByteString)] -> IO ()
putEmails MboxFmt       = B.putStr . showMbox . Mbox . map snd
--putEmails (FmtComb fmt) = mapM_ (B.putStrLn . renderFmtComb fmt) -- it's seems to compute a big part (all?) of the list before starting to print (when using mbox-grep for instance)
putEmails (FmtComb fmt) = mapM_ (putStrLnB' . renderFmtComb fmt)

