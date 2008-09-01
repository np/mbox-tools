--------------------------------------------------------------------
-- |
-- Module    : Debug.TraceS
-- Copyright : (c) Nicolas Pouillard 2008
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

module Debug.TraceS (traceS) where

traceS :: Show a => a -> a
traceS x = trace (show x) x

