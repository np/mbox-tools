module Mbox (Mbox(..), MboxMessage(..), mboxCountMessages) where

newtype Mbox s = Mbox { unMbox :: [MboxMessage s] }
data MboxMessage s = MboxMessage { mboxMsgSender :: s, mboxMsgTime :: s, mboxMsgBody :: s }

mboxCountMessages :: Mbox s -> Int
mboxCountMessages = length . unMbox

