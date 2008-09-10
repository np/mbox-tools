{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------
-- |
-- Module    : Email
-- Copyright : (c) Nicolas Pouillard 2008
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
module Email where

import Control.Applicative
import Control.Arrow
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as B
import Codec.MIME.Type (MIMEValue(..), Type(..))
import Codec.MIME.Parse (parseMIMEBody)
import Text.ParserCombinators.Parsec.Rfc2822 (Field(..), fields)
import Text.Parsec.ByteString.Lazy ()
import Text.Parsec.Prim (parse)
import EOL (fixCrlfS) -- fixCrlfB
import System.Console.GetOpt (OptDescr(..),ArgDescr(..))

data Email = Email { emailFields  :: [Field]
                   , emailContent :: MIMEValue
                   , rawEmail :: B.ByteString
                   }
  deriving (Show)

data ShowFormat = OneLinerDebug
                | MboxFmt
  deriving (Eq,Enum)

showFmts :: [ShowFormat]
showFmts = [ OneLinerDebug .. ] -- since OneLinerDebug is the first

instance Show ShowFormat where
  show OneLinerDebug = "one"
  show MboxFmt       = "mbox"

myCunpack :: C.ByteString -> String
myCunpack = C.unpack
--myCunpack = unfoldr C.uncons
--myCunpack ys = case C.uncons ys of { Nothing -> [] ; Just (x,xs) -> x : myCunpack xs }


{-
  Since parsing the full message is two slow, so one first extract fields.

  -- the old version
  > readEmail = either (error . show) id . parse message "<string>" . fixCrlfS . myCunpack
 -}

readFields :: B.ByteString -> [Field]
readFields input =
  either err id . parse fields "<string>" . fixCrlfS . myCunpack $ input
  where err e = error $ "Error in the following message <<EOS\n" ++ myCunpack input ++ "\nEOS\n\n" ++ show e

{-
readField :: B.ByteString -> Field
readField input =
  either err id . parse field "<string>" . myCunpack $ input
  where err e = error $ "Error in the following line:\n  " ++ show (myCunpack input) ++ "\n\n" ++ show e

readFields :: B.ByteString -> [Field]
readFields = map (readField . (`C.append` (C.pack "\r\n"))) . C.lines
-}

readEmail :: B.ByteString -> Email
readEmail !orig = mkEmail $ maybe (error "readEmail: parse error") id $ splitAtNlNl 0 orig
  where splitAtNlNl !count !input = do
          off <- (+1) <$> C.elemIndex '\n' input
          let i' = C.drop off input
          if C.head i' == '\n'
           then Just (C.take (off + count) orig, C.tail i')
           else splitAtNlNl (off + count) i'
        mkEmail ~(flds, body) =
          Email { emailFields = headers
                , emailContent = parseMIMEBody optional_headers (myCunpack body)
                , rawEmail = orig }
          where headers = readFields flds
                optional_headers = [ (k,v) | OptionalField k v <- headers ]

ellipse :: Int -> String -> String
ellipse n s = take n s ++ "..."

showEmailAsOneLinerDebug :: Email -> String
showEmailAsOneLinerDebug (msg@(Email flds content _)) =
          take 30 (show (mimeType $ mime_val_type content) ++ repeat ' ') ++ " Subject: " ++
          head ([show subject | Subject subject <- flds ]
              ++ ["(malformed subject) " ++ show subject | OptionalField "Subject" subject <- flds ]
              ++ ["no subject, body: " ++ ellipse 40 (show msg)])

fmtOpt :: (forall err. String -> err) -> (ShowFormat -> a) -> OptDescr a
fmtOpt usage f = Option ['f'] ["fmt"] (ReqArg (f . parseFmt) "FMT") desc
  where parseFmt = maybe (usage "Bad display format") id . (`lookup` fmts)
        fmts = map (show &&& id) showFmts
        desc = "Choose the display format " ++ show (map fst fmts)

