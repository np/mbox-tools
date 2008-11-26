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
import Codec.MIME.Type (MIMEValue(..), MIMEValueB, Type(..))
import Codec.MIME.Parse (parseMIMEBody, safeParseMIMEBodyByteString, WithoutCRLF(..))
import Text.ParserCombinators.Parsec.Rfc2822 (Field(..), fields)
import Text.ParserCombinators.Parsec (parse)
import EOL (fixCrlfS, fixCrlfB)
import System.Console.GetOpt (OptDescr(..),ArgDescr(..))
import System.IO.Error (ioError, catch, isDoesNotExistError)
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)
import Mbox (Mbox(Mbox), MboxMessage, printMbox, printMboxFromLine)

data Email = Email { emailFields  :: [Field]
                   , emailContent :: MIMEValueB
                   , rawEmail     :: B.ByteString
                   }
  deriving (Show)

data ShowFormat = OneLinerDebug
                | MboxFmt
                | MboxFrom
  deriving (Eq,Enum)

showFmts :: [ShowFormat]
showFmts = [ OneLinerDebug .. ] -- since OneLinerDebug is the first

instance Show ShowFormat where
  show OneLinerDebug = "one"
  show MboxFmt       = "mbox"
  show MboxFrom      = "from"

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

safeGetEnv :: String -> IO (Maybe String)
safeGetEnv s = (Just <$> getEnv s) `catch` \e -> if isDoesNotExistError e
                                                 then return Nothing
                                                 else ioError e -- I'm wondering if this could happen

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

readEmail :: B.ByteString -> Email
readEmail !orig = mkEmail $ maybe (error "readEmail: parse error") id $ splitAtNlNl 0 orig
  where splitAtNlNl !count !input = do
          off <- (+1) <$> C.elemIndex '\n' input
          let i' = C.drop off input
          case C.uncons i' of
            Nothing          -> Just (C.take (off + count) orig, C.empty)
            Just ('\n', i'') -> Just (C.take (off + count) orig, i'')
            _ -> splitAtNlNl (off + count) i'
        mkEmail ~(flds, body) =
          Email { emailFields = headers
                --, emailContent = parseMIMEBody optional_headers (fixCrlfB body)
                --, emailContent = fmap C.pack $ parseMIMEBody optional_headers (fixCrlfS (C.unpack body))
                --, emailContent = safeParseMIMEBodyByteString optional_headers (fixCrlfB body)
                , emailContent = dynParseMIMEBody optional_headers body
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

putEmails :: ShowFormat -> [(Email,MboxMessage B.ByteString)] -> IO ()
putEmails OneLinerDebug = mapM_ (putStrLn . showEmailAsOneLinerDebug . fst)
putEmails MboxFmt       = B.putStr . printMbox . Mbox . map snd
putEmails MboxFrom      = mapM_ (B.putStr . printMboxFromLine . snd)

