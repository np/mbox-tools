--------------------------------------------------------------------
-- |
-- Executable : redact-mbox
-- Copyright : (c) Nicolas Pouillard 2008, 2009, 2011
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

import Codec.Mbox (Mbox(..),MboxMessage(..),parseMbox,showMbox)
import qualified Data.ByteString.Lazy as B (ByteString,readFile,writeFile)
import qualified Data.ByteString.Lazy.Char8 as C (mapAccumL,unpack,pack)
import Data.List (mapAccumL)
import Data.Char (isDigit,isLower,isUpper)
import Control.Applicative
import Control.Monad.State
import System.Environment (getArgs)
import System.Random (getStdGen,randomRs)
import System.IO

type RedactState a = State [Int] a

-- BEGIN MISSING
consume :: State [a] a
consume = do (c:cs) <- get
             put cs
             return c

infix !!!
(!!!) :: [a] -> Int -> a
[] !!! _ = error "!!!: empty list"
lst !!! idx = go lst idx
  where go []     n       = go lst n
        go (x:_)  0       = x
        go (_:xs) (n + 1) = go xs n
        go _      _       = error "!!!: negative index"

-- prop_nthmod (NonNegative n) (NonEmpty xs) = xs !!! n == xs !! (n `mod` length xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

wrapState :: (acc -> x -> (acc, y)) -> x -> State acc y
wrapState f x = state (\s -> swap (f s x))

unwrapState :: (x -> State acc y) -> acc -> x -> (acc, y)
unwrapState f s x = swap (runState (f x) s)
-- END MISSING

redactChar :: Char -> RedactState Char
redactChar c | c `elem` " \t\n!@#$%^&*()[]{}/=?+-_:;|\\" = return c
             | isDigit c                                 = (digits !!!) <$> consume
             | isLower c                                 = (lowers !!!) <$> consume
             | isUpper c                                 = (uppers !!!) <$> consume
             | otherwise                                 = (alphas !!!) <$> consume

digits, alphas, lowers, uppers :: [Char]
digits = ['0'..'9']
lowers = ['a'..'z']
uppers = ['A'..'Z']
alphas = lowers ++ uppers

redactString :: B.ByteString -> RedactState B.ByteString
redactString = wrapState $ C.mapAccumL $ unwrapState redactChar

{- NOTE that the offset is not redacted -}
redactMboxMessage :: MboxMessage B.ByteString -> RedactState (MboxMessage B.ByteString)
redactMboxMessage (MboxMessage sender time body file offset) =
  MboxMessage `fmap` redactString sender `ap` redactString time `ap` redactString body
                `ap` (C.unpack `fmap` redactString (C.pack file)) `ap` return offset

main :: IO ()
main = do
  args <- getArgs
  ints <- randomRs (0, length digits + length alphas) <$> getStdGen
  case args of
    [argin,argout] ->
        B.writeFile argout
          =<< return . showMbox . Mbox . snd . mapAccumL (unwrapState redactMboxMessage) ints . mboxMessages . parseMbox
          =<< B.readFile argin
    _ -> hPutStrLn stderr "Usage: redact-mbox <in.mbox> <out.mbox>"

