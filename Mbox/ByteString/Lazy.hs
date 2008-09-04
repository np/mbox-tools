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
module Mbox.ByteString.Lazy (parseMbox,fromQuoting) where

import Mbox (Mbox(..), MboxMessage(..))
import Control.Arrow ((***),first)
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy.Char8 as C -- Char8 interface over Lazy ByteString's
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)

--import Test.QuickCheck
--import QuickCheckUtils

nextFrom :: ByteString -> Maybe (ByteString, ByteString)
nextFrom !orig = goNextFrom 0 orig
   where goNextFrom !count !input = do
           off <- (+1) <$> C.elemIndex '\n' input
           let i' = C.drop off input
           if C.take 5 i' == bFrom
            then Just (C.take (off + count) orig, C.drop 5 i')
            else goNextFrom (off + count) i'

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
-}

mkQuotedFrom :: Int64 -> C.ByteString
mkQuotedFrom n | n < 0     = error "mkQuotedFrom: negative quoting"
               | otherwise = '\n' `C.cons` C.replicate n '>' `C.append` bFrom


bFrom :: ByteString
bFrom = C.pack "From "

skipFirstFrom :: ByteString -> ByteString
skipFirstFrom xs | bFrom == C.take 5 xs = C.drop 5 xs
                 | otherwise = error "skipFirstFrom: badly formatted mbox: 'From ' expected at the beginning"

parseMbox :: ByteString -> Mbox ByteString
parseMbox = Mbox . go . skipFirstFrom
  where go !input =
          case nextFrom input of
            Nothing -> if C.null input then [] else [finishMboxMessageParsing input]
            Just (msg, rest) -> finishMboxMessageParsing msg : go rest
        finishMboxMessageParsing inp = MboxMessage sender time (fromQuoting pred body)
            where ((sender,time),body) = C.break (==' ') *** C.tail $ C.break (=='\n') inp

