--------------------------------------------------------------------
-- |
-- Executable : redact-mbox
-- Copyright : (c) Nicolas Pouillard 2008
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

import Codec.Mbox (Mbox(..),MboxMessage(..),parseMbox,printMbox)
import qualified Data.ByteString.Lazy as B (ByteString,readFile,writeFile)
import qualified Data.ByteString.Lazy.Char8 as C (mapAccumL)
import Data.List (mapAccumL)
import Data.Char (isDigit,isLower,isUpper)
import Control.Applicative
import Control.Monad.State
import System.Environment (getArgs)
import System.Random (getStdGen,randomRs)
import System.IO

type RedactState a = State [Int] a

-- BEGIN MISSING
{-
list :: b -> (a -> [a] -> b) -> [a] -> b
list nil _    []     = nil
list _   cons (x:xs) = cons x xs
-}

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
wrapState f x = State (\state -> swap (f state x))

unwrapState :: (x -> State acc y) -> acc -> x -> (acc, y)
unwrapState f state x = swap (runState (f x) state)
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

redactMboxMessage :: MboxMessage B.ByteString -> RedactState (MboxMessage B.ByteString)
redactMboxMessage (MboxMessage sender time body) =
  -- MboxMessage <$> redactString sender <*> redactString time <*> redactString body
  MboxMessage `fmap` redactString sender `ap` redactString time `ap` redactString body

main :: IO ()
main = do
  args <- getArgs
  ints <- randomRs (0, length digits + length alphas) <$> getStdGen
  case args of
    [argin,argout] ->
        B.writeFile argout
          =<< return . printMbox . Mbox . snd . mapAccumL (unwrapState redactMboxMessage) ints . unMbox . parseMbox
          =<< B.readFile argin
    _ -> hPutStrLn stderr "Usage: redact-mbox <in.mbox> <out.mbox>"

