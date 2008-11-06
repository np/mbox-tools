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

module Mbox (Mbox(..), MboxMessage(..), mboxMsgSenderA, mboxMsgTimeA, mboxMsgBodyA) where

import Data.Accessor

newtype Mbox s = Mbox { unMbox :: [MboxMessage s] }
  deriving (Eq, Ord, Show)
data MboxMessage s = MboxMessage { mboxMsgSender :: s, mboxMsgTime :: s, mboxMsgBody :: s }
  deriving (Eq, Ord, Show)

mboxMsgSenderA :: Accessor (MboxMessage s) s
mboxMsgSenderA = accessor mboxMsgSender (\x r -> r{mboxMsgSender=x})

mboxMsgTimeA :: Accessor (MboxMessage s) s
mboxMsgTimeA = accessor mboxMsgTime (\x r -> r{mboxMsgTime=x})

mboxMsgBodyA :: Accessor (MboxMessage s) s
mboxMsgBodyA = accessor mboxMsgBody (\x r -> r{mboxMsgBody=x})
