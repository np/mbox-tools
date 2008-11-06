--------------------------------------------------------------------
-- |
-- Module    : Mbox.ByteString.Lazy
-- Copyright : (c) Nicolas Pouillard 2008
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Mbox.ByteString.Lazy
  ( Direction(..)
  , parseMboxFile
  , parseMbox
  , safeParseMbox
  , printMbox
  , printMboxMessage
  , printMboxFromLine
  , fromQuoting
  , readRevMboxFile
  ) where

import Mbox (Mbox(..), MboxMessage(..))
import Control.Arrow (first,second)
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy.Char8 as C -- Char8 interface over Lazy ByteString's
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)

--import Test.QuickCheck

nextFrom :: ByteString -> Maybe (ByteString, ByteString)
nextFrom !orig = goNextFrom 0 orig
   where goNextFrom !count !input = do
           off <- (+1) <$> C.elemIndex '\n' input
           let (nls, i') = first C.length $ C.span (=='\n') $ C.drop off input
           if C.take 5 i' == bFrom
            then Just (C.take (off + count + (nls - 1)) orig, C.drop 5 i')
            else goNextFrom (off + count + nls) i'

{-
Quoted from http://qmail.org./man/man5/mbox.html:

  >From quoting ensures that the resulting
  lines are not From_ lines:  the program prepends a > to any
  From_ line, >From_ line, >>From_ line, >>>From_ line, etc.
-}

-- TODO rules:
--  - fromQuoting id == id
--  - f . g == id ==> fromQuoting f . fromQuoting g == id
--    but fromQuoting can fail on negative levels
--  special case that works:
--     n > 0 ==> fromQuoting ((-)n) . fromQuoting (+n) == id

-- TODO performances:
--   This ByteString fromQuoting is already quite fast,
--   It still implies a x2 factor to the mbox-average-size tool, wether one
--   enables it or not.
--   Perhaps fusing nextFrom and fromQuoting could give more performances
--   (but will decrease performances of mbox-counting).

fromQuoting :: (Int64 -> Int64) -> C.ByteString -> C.ByteString
fromQuoting onLevel = C.tail . nextQuotedFrom . C.cons '\n'
  where nextQuotedFrom !orig = goNextQuotedFrom 0 orig
         where goNextQuotedFrom !count !input =
                case C.elemIndex '\n' input of
                 Nothing -> orig
                 Just off ->
                   let (!level, i') = first C.length $ C.span (=='>') $ C.drop (off + 1) input in
                   if C.take 5 i' == bFrom
                    then (C.take (off + count) orig) `C.append`
                         mkQuotedFrom (onLevel level) `C.append`
                         nextQuotedFrom (C.drop 5 i')
                    else goNextQuotedFrom (off + level + count + 1) i'

{-
prop_fromQuotingInv (NonNegative n) s = s == fromQuoting (+(-n)) (fromQuoting (+n) s)
prop_unparse_parse m = either (const False) (==m) $ safeParseMbox (printMbox m)
prop_parse_unparse m = let s = printMbox m in Right s == (printMbox <$> safeParseMbox s)

-- | Prefered xs: have the xs elements as favorites.
newtype Prefered a = Prefered { unPrefered :: a }
 deriving ( Eq, Ord, Show, Read )

class Favorites a where
  favorites :: [a]

instance Favorites Char where
  favorites = ">From \n"

instance Arbitrary C.ByteString where
    arbitrary = arbitrary >>= return . C.pack . map unPrefered

instance (Favorites a, Arbitrary a) => Arbitrary (Prefered a) where
  arbitrary =
    frequency
      [ (2, Prefered `fmap` oneof (map return favorites))
      , (1, Prefered `fmap` arbitrary)
      ]

  shrink (Prefered x) = Prefered `fmap` shrink x

--instance Arbitrary s => Arbitrary (MboxMessage s) where
instance Arbitrary (MboxMessage ByteString) where
  arbitrary = MboxMessage (C.pack "S") (C.pack "D") <$> arbitrary -- TODO better sender,time
  shrink (MboxMessage x y z) = [ MboxMessage x' y' z' | (x', y', z') <- shrink (x, y, z) ]

instance Arbitrary (Mbox ByteString) where
--instance Arbitrary s => Arbitrary (Mbox s) where
  arbitrary = Mbox <$> arbitrary
  shrink (Mbox x) = Mbox <$> shrink x
-}

mkQuotedFrom :: Int64 -> C.ByteString
mkQuotedFrom n | n < 0     = error "mkQuotedFrom: negative quoting"
               | otherwise = '\n' `C.cons` C.replicate n '>' `C.append` bFrom


bFrom :: ByteString
bFrom = C.pack "From "

skipFirstFrom :: ByteString -> Either String ByteString
skipFirstFrom xs | bFrom == C.take 5 xs = Right $ C.drop 5 xs
                 | otherwise = Left "skipFirstFrom: badly formatted mbox: 'From ' expected at the beginning"

safeParseMbox :: ByteString -> Either String (Mbox ByteString)
safeParseMbox s | C.null s  = Right $ Mbox []
                | otherwise = (Mbox . map finishMboxMessageParsing . splitMboxMessages) <$> (skipFirstFrom s)

parseMbox :: ByteString -> Mbox ByteString
parseMbox = either error id . safeParseMbox

splitMboxMessages :: ByteString -> [ByteString]
splitMboxMessages !input =
  case nextFrom input of
    Nothing | C.null input -> []
            | otherwise    -> [input]
    Just (!msg, rest)      -> msg : splitMboxMessages rest

finishMboxMessageParsing :: ByteString -> MboxMessage ByteString
finishMboxMessageParsing !inp = MboxMessage sender time (fromQuoting pred body)
  where ((sender,time),body) = first (breakAt ' ') $ breakAt '\n' inp
        breakAt c = second (C.drop 1 {- a safe tail -}) . C.break (==c)

printMbox :: Mbox ByteString -> ByteString
printMbox = C.intercalate (C.pack "\n") . map printMboxMessage . unMbox

printMboxFromLine :: MboxMessage ByteString -> ByteString
printMboxFromLine (MboxMessage sender time _) =
  C.append bFrom
    $ C.append sender
    $ C.cons   ' '
    $ C.append time
    $ C.cons   '\n'
    $ C.empty

printMboxMessage :: MboxMessage ByteString -> ByteString
printMboxMessage msg = printMboxFromLine msg `C.append` fromQuoting (+1) (mboxMsgBody msg)

readRevMboxFile :: FilePath -> IO (Mbox ByteString)
readRevMboxFile fn = readRevMboxHandle =<< openFile fn ReadMode

-- | @readRevMboxHandle h@ returns a reversed mbox for a file handle.
-- The file handle is supposed to be in text mode, readable.
-- buffering?
readRevMboxHandle :: Handle -> IO (Mbox ByteString)
readRevMboxHandle fh = readRevMbox <$> readHandleBackward fh

readRevMbox :: [ByteString] -> Mbox ByteString
readRevMbox chunks = Mbox $ go (filter (not . C.null) chunks)
  where go []          = []
        go (chunk1:cs) =
                  case nextFrom chunk1 of
                    Nothing           -> kont cs chunk1
                    Just (!msg, rest) ->
                      (map finishMboxMessageParsing . reverse . splitMboxMessages $ rest) ++ kont cs msg

        kont []           = (:[]) . finishLast
        kont (chunk2:cs2) = \k -> go (chunk2 `C.append` k : cs2)

        finishLast = either (error . ("readRevMboxHandle: impossible: " ++)) finishMboxMessageParsing
                   . skipFirstFrom

-- | @readHandleBackward h@ lazily reads a file handle from the end of the
-- file. The file contents is returned as a reversed list of chunks.
-- The result is such that if one apply @C.concat . reverse@ one get
-- the in-order contents.
{-
propIO_read_anydir fh =
   do xs <- hGetContent fh
      ys <- readHandleBackward fh
      xs == C.concat (reverse ys)
-}
readHandleBackward :: Handle -> IO [ByteString]
readHandleBackward fh = hFileSize fh >>= go
  where go 0   = return []
        go siz = unsafeInterleaveIO $
          do let delta = min mboxChunkSize siz
                 siz'  = siz - delta
             hSeek fh AbsoluteSeek siz'
             s <- C.hGet fh $ fromInteger delta
             (s :) <$> go siz'

data Direction = Backward | Forward

parseMboxFile :: Direction -> FilePath -> IO (Mbox ByteString)
parseMboxFile Forward  = (either fail return =<<) . (safeParseMbox <$>) . C.readFile
parseMboxFile Backward = readRevMboxFile

mboxChunkSize :: Integer
mboxChunkSize = 10*oneMegabyte

-- one megabyte in bytes
oneMegabyte :: Integer
oneMegabyte = 2 ^ (20 :: Int)
