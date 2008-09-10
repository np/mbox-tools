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

module Debug.TraceS (trace,traceS) where

import Debug.Trace (trace)

traceS :: Show a => a -> a
traceS x = trace (show x) x

