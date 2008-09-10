--------------------------------------------------------------------
-- |
-- Module    : Mbox
-- Copyright : (c) Nicolas Pouillard 2008
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

module Mbox (Mbox(..), MboxMessage(..)) where

newtype Mbox s = Mbox { unMbox :: [MboxMessage s] }
  deriving (Eq, Ord, Show)
data MboxMessage s = MboxMessage { mboxMsgSender :: s, mboxMsgTime :: s, mboxMsgBody :: s }
  deriving (Eq, Ord, Show)
