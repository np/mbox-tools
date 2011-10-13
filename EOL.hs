--------------------------------------------------------------------
-- |
-- Module    : EOL
-- Copyright : (c) Nicolas Pouillard 2008, 2009, 2010, 2011
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

{-# LANGUAGE PatternGuards, BangPatterns #-}
module EOL where

import qualified Data.ByteString.Lazy.Char8 as C

-- Make sure all lines are terminated by CRLF.

fixCrlfS :: String -> String
fixCrlfS ('\r':'\n':xs)   = '\r' : '\n' : fixCrlfS xs
fixCrlfS ('\n':xs)        = '\r' : '\n' : fixCrlfS xs
fixCrlfS (x:xs)           = x : fixCrlfS xs
fixCrlfS []               = []

fixLfS :: String -> String
fixLfS ('\r':'\n':xs)   = '\n' : fixLfS xs
fixLfS (x:xs)           = x : fixLfS xs
fixLfS []               = []

fixCrlfB :: C.ByteString -> C.ByteString

fixCrlfB = C.unlines . map (`C.snoc` '\r') . C.lines

{-
fixCrlfB = go
  where go !input = maybe C.empty f $ C.elemIndex '\n' input
            where f !0   = crlf `C.append` go (C.tail input)
                  f !off | C.index input (off - 1) == '\r' =
                                let (a,b) = C.splitAt (off+1) input in a `C.append` go b
                         | otherwise                       =
                                let (a,b) = C.splitAt off input in a `C.append` (crlf `C.append` go (C.tail b))

By speed order:

fixCrlfB = go
  where go input = maybe C.empty f $ C.elemIndex '\n' input
            where f 0   = crlf `C.append` go (C.tail input)
                  f off | C.index input (off - 1) == '\r' =
                           uncurry C.append $ second go $ C.splitAt (off + 1) input
                        | otherwise =
                           uncurry C.append $ second (C.append crlf . go . C.tail) $ C.splitAt off input

fixCrlfB = go
  where go input = maybe C.empty f $ C.elemIndex '\n' input
            where f 0   = '\r' `C.cons` ('\n' `C.cons` go (C.tail input))
                  f off | C.index input (off - 1) == '\r' =
                           uncurry C.append $ second go $ C.splitAt (off + 1) input
                        | otherwise =
                           let (a, b) = C.splitAt off input in a `C.append` ('\r' `C.cons` ('\n' `C.cons` go (C.tail b)))
fixCrlfB = go
  where go s1 =
          case C.uncons s1 of
            Just ('\r', s2) | Just ('\n', rest) <- C.uncons s2 -> '\r' `C.cons` ('\n' `C.cons` go rest)
            Just ('\n', rest)                                  -> '\r' `C.cons` ('\n' `C.cons` go rest)
            Just (c, rest)                                     -> c `C.cons` go rest
            Nothing -> C.empty
-}

crlf :: C.ByteString
crlf = C.pack "\r\n"
