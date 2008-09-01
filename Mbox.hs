module Mbox (Mbox(..), MboxMessage(..), mboxCountMessages) where

newtype Mbox s = Mbox { unMbox :: [MboxMessage s] }
  deriving (Eq, Ord, Show)
data MboxMessage s = MboxMessage { mboxMsgSender :: s, mboxMsgTime :: s, mboxMsgBody :: s }
  deriving (Eq, Ord, Show)

mboxCountMessages :: Mbox s -> Int
mboxCountMessages = length . unMbox

{-
printMbox = concatMap printMsg
  where printMsg ((s,t),Message flds body) =
           "From "++s++" "++t++nl++concatMap printField flds++body++nl
        printField f = show f ++ nl
        nl = "\r\n"
-}

