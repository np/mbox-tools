module Mbox (Mbox(..), MboxMessage(..), mboxCountMessages) where

newtype Mbox s = Mbox { unMbox :: [MboxMessage s] }
  deriving (Eq, Ord, Show)
data MboxMessage s = MboxMessage { mboxMsgSender :: s, mboxMsgTime :: s, mboxMsgBody :: s }
  deriving (Eq, Ord, Show)

mboxCountMessages :: Mbox s -> Int
mboxCountMessages = length . unMbox

