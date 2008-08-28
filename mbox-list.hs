{-# LANGUAGE BangPatterns #-}
import Control.Applicative
import Control.Arrow
import Mbox (Mbox(..), mboxMsgBody)
import Mbox.ByteString.Lazy (parseMbox)
import Text.ParserCombinators.Parsec.Rfc2822 (Field(..), Message(..), fields, message)
import Text.Parsec.ByteString.Lazy ()
import Text.Parsec.Prim (parse)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment
import Debug.Trace (trace)
traceS :: Show a => a -> a
-- traceS x = trace (show x) x
traceS = flip trace <*> show

-- Make sure all lines are terminated by CRLF.

fixEol :: String -> String
fixEol ('\r':'\n':xs)   = '\r' : '\n' : (fixEol xs)
fixEol ('\n':xs)        = '\r' : '\n' : (fixEol xs)
fixEol (x:xs)           = x : (fixEol xs)
fixEol []               = []

unfixEol :: String -> String
unfixEol ('\r':'\n':xs)   = '\n' : unfixEol xs
unfixEol (x:xs)           = x : unfixEol xs
unfixEol []               = []

printMbox = concatMap printMsg
  where printMsg ((s,t),Message flds body) =
           "From "++s++" "++t++nl++concatMap printField flds++body++nl
        printField f = show f ++ nl
        nl = "\r\n"

readMessage = either (error . show) id . parse message "<string>" . fixEol . C.unpack

ellipse :: Int -> String -> String
ellipse n s = take n s ++ "..."

main :: IO ()
main = do
  [arg] <- getArgs
  input <- B.readFile arg
  mapM_ (putStrLn . subjectOf . readMessage . mboxMsgBody) . unMbox . parseMbox $ input -- (fixEol input)
  where subjectOf (msg@(Message flds body)) =
          head $ [show subject | Subject subject <- flds ]
              ++ ["(malformed subject) " ++ show subject | OptionalField "Subject" subject <- flds ]
              ++ ["no subject, body: " ++ ellipse 40 (show msg)]

