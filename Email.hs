{-# LANGUAGE BangPatterns #-}
module Email where

import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as B
import Codec.MIME.Type (MIMEValue(..), Type(..))
import Codec.MIME.Parse (parseMIMEBody)
import Text.ParserCombinators.Parsec.Rfc2822 (Field(..), fields)
import Text.Parsec.ByteString.Lazy ()
import Text.Parsec.Prim (parse)
import EOL (fixCrlfS) -- fixCrlfB

data Email = Email { emailFields  :: [Field]
                   , rawEmail     :: B.ByteString
                   , emailContent :: MIMEValue }
  deriving (Show)

data ShowFormat = OneLinerDebug

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
readEmail !orig = maybe (error "readEmail: parse error") id $ splitAtNlNl 0 orig
  where splitAtNlNl !count !input = do
          off <- (+1) <$> C.elemIndex '\n' input
          let i' = C.drop off input
          if C.head i' == '\n'
           then Just $ mkEmail (C.take (off + count) orig) (C.tail i')
           else splitAtNlNl (off + count) i'
        mkEmail flds body =
          Email headers orig (parseMIMEBody optional_headers (myCunpack body))
          where headers = readFields flds
                optional_headers = [ (k,v) | OptionalField k v <- headers ]

ellipse :: Int -> String -> String
ellipse n s = take n s ++ "..."

showEmail :: ShowFormat -> Email -> String
showEmail OneLinerDebug (msg@(Email flds _ content)) =
          take 30 (show (mimeType $ mime_val_type content) ++ repeat ' ') ++ " Subject: " ++
          head ([show subject | Subject subject <- flds ]
              ++ ["(malformed subject) " ++ show subject | OptionalField "Subject" subject <- flds ]
              ++ ["no subject, body: " ++ ellipse 40 (show msg)])
