{-# LANGUAGE BangPatterns #-}
module Mbox.ByteString.Lazy (parseMbox) where
import Mbox (Mbox(..), MboxMessage(..))
import Control.Arrow (first)
import qualified Data.ByteString.Lazy.Char8 as C -- Char8 interface over Lazy ByteString's
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)

nextFrom :: ByteString -> Maybe (Int64, ByteString)
nextFrom = goNextFrom 0
   where goNextFrom !count !input = do
           off <- C.elemIndex '\n' input
           let i' = C.drop (off + 1) input
           if C.take 5 i' == bFrom
            then Just (off + count, C.drop 5 i')
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
            Just (off, rest) -> finishMboxMessageParsing (C.take off input) : go rest
        finishMboxMessageParsing inp = MboxMessage sender time body
            where ((sender,time),body) = first (C.break (==' ')) (C.break (=='\n') inp)
