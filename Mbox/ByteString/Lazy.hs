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
module Mbox.ByteString.Lazy (parseMbox) where

import Mbox (Mbox(..), MboxMessage(..))
import Control.Arrow ((***))
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy.Char8 as C -- Char8 interface over Lazy ByteString's
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)

nextFrom :: ByteString -> Maybe (ByteString, ByteString)
nextFrom !orig = goNextFrom 0 orig
   where goNextFrom !count !input = do
           off <- (+1) <$> C.elemIndex '\n' input
           let i' = C.drop off input
           if C.take 5 i' == bFrom
            then Just (C.take (off + count) orig, C.drop 5 i')
            else goNextFrom (off + count) i'

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
        finishMboxMessageParsing inp = MboxMessage sender time body
            where ((sender,time),body) = C.break (==' ') *** C.tail $ C.break (=='\n') inp
