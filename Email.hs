--------------------------------------------------------------------
-- |
-- Module    : Email
-- Copyright : (c) Nicolas Pouillard 2008, 2009, 2010, 2011
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

{-# LANGUAGE BangPatterns, Rank2Types, TemplateHaskell, TypeOperators,
             OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Email where

import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as B
{- TMP-NO-MIME
import Codec.MIME.Type (MIMEValueB)
import Codec.MIME.Parse (parseMIMEBody, safeParseMIMEBodyByteString, WithoutCRLF(..))
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)
-}
import Text.ParserCombinators.Parsec.Rfc2822 (Field(..), fields)
import Text.ParserCombinators.Parsec (parse)
import EOL (fixCrlfS {- TMP-NO-MIME, fixCrlfB -})
import System.IO.Error (isDoesNotExistError)
import System.Environment (getEnv)
import Codec.Mbox (Mbox(..), mboxMsgBody)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Char (toLower)
import Data.Label

data Email = Email { _emailFields  :: [Field]
                   -- TMP-NO-MIME , _emailContent :: MIMEValueB
                   , _emailContent :: B.ByteString
                   , _rawEmail     :: B.ByteString
                   }
  deriving (Show)

$(mkLabels [''Email])

myCunpack :: C.ByteString -> String
myCunpack = C.unpack
--myCunpack = unfoldr C.uncons
--myCunpack ys = case C.uncons ys of { Nothing -> [] ; Just (x,xs) -> x : myCunpack xs }

{-
  Since parsing the full message is two slow, one first extract fields.

  -- the old version
  > readEmail = either (error . show) id . parse message "<string>" . fixCrlfS . myCunpack
 -}

readFields :: B.ByteString -> [Field]
readFields input =
  either err id . parse fields "<string>" . fixCrlfS . myCunpack $ input
  where err e = error $ "Error in the following message <<EOS\n" ++ myCunpack input ++ "\nEOS\n\n" ++ show e

-- | Takes a message and reads only the fields part of it.
readFieldsOnly :: B.ByteString -> [Field]
readFieldsOnly inp = readFields . fst . fromMaybe err . splitAtNlNl $ inp
  where err = error "readFieldsOnly: parse error"

{-
readField :: B.ByteString -> Field
readField input =
  either err id . parse field "<string>" . myCunpack $ input
  where err e = error $ "Error in the following line:\n  " ++ show (myCunpack input) ++ "\n\n" ++ show e

readFields :: B.ByteString -> [Field]
readFields = map (readField . (`C.append` (C.pack "\r\n"))) . C.lines
-}

safeGetEnv :: String -> IO (Maybe String)
safeGetEnv s = (Just <$> getEnv s) `catch` \e -> if isDoesNotExistError e
                                                 then return Nothing
                                                 else ioError e -- I'm wondering if this could happen

{- TMP-NO-MIME
{-# NOINLINE dynParseMIMEBody #-}
dynParseMIMEBody :: [(String, String)] -> B.ByteString -> MIMEValueB
dynParseMIMEBody = unsafePerformIO $
 do mode <- safeGetEnv "MIME_PARSING_MODE"
    return $ case mode of
      Just "string"         -> \hdr -> fmap C.pack . parseMIMEBody hdr . fixCrlfS . C.unpack
      Just "bytestring"     -> \hdr -> parseMIMEBody hdr . fixCrlfB
      Just "safe"           -> \hdr -> safeParseMIMEBodyByteString hdr . fixCrlfB
      Just "bytestringcrlf" -> \hdr -> fmap withoutCRLF . parseMIMEBody hdr . WithoutCRLF
      Just other            -> trace ("unknown MIME_PARSING_MODE " ++ other) parseMIMEBody
      Nothing               -> parseMIMEBody
-}

splitAtNlNl :: B.ByteString -> Maybe (B.ByteString, B.ByteString)
splitAtNlNl !orig = go 0 orig
  where go !count !input = do
          off <- (+1) <$> C.elemIndex '\n' input
          let i' = C.drop off input
          case C.uncons i' of
            Nothing          -> Just (C.take (off + count) orig, C.empty)
            Just ('\n', i'') -> Just (C.take (off + count) orig, i'')
            _ -> go (off + count) i'

readEmail :: B.ByteString -> Email
readEmail !orig = mkEmail $ fromMaybe (error "readEmail: parse error") $ splitAtNlNl orig
     where
        mkEmail ~(flds, body) =
          Email { _emailFields = headers
                --, emailContent = parseMIMEBody optional_headers (fixCrlfB body)
                --, emailContent = fmap C.pack $ parseMIMEBody optional_headers (fixCrlfS (C.unpack body))
                --, emailContent = safeParseMIMEBodyByteString optional_headers (fixCrlfB body)
                {- TMP-NO-MIME
                , _emailContent = dynParseMIMEBody optional_headers body
                -}
                , _emailContent = body
                , _rawEmail = orig }
          where headers = readFields flds
          {- TMP-NO-MIME
                optional_headers = [ (k,v) | OptionalField k v <- headers ]
          -}

readMboxEmails :: Mbox B.ByteString -> [Email]
readMboxEmails = map (readEmail . get mboxMsgBody) . mboxMessages

stringOfField :: Field -> (String, String)
stringOfField (MessageID x) = ("message-id", fromMaybe (error "impossible: Email.stringOfField") $ unquote x)
stringOfField (Subject   x) = ("subject",    x)
stringOfField (OptionalField x y) = (map toLower x, y)
stringOfField x = ("x-unknown", show x) -- TODO

messageId :: Email -> Maybe String
messageId msg = listToMaybe [ mid | MessageID mid <- get emailFields msg ]

messageSubject :: Email -> Maybe String
messageSubject msg = listToMaybe [ mid | Subject mid <- get emailFields msg ]
{-
  head ([show subject | Subject subject <- flds ]
      ++ ["(malformed subject) " ++ show subject | OptionalField "Subject" subject <- flds ]
      ++ ["no subject, body: " ++ ellipse 40 (show msg)])
-}
unquote :: String -> Maybe String
unquote ('<':xs) = listToMaybe [ys | zs == ">"] where (ys, zs) = break (=='>') xs
unquote _        = Nothing
