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

import Control.Applicative hiding (Const)
import Control.Arrow
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Internal as B
import Codec.MIME.Type (MIMEValue(..), MIMEValueB, Type(..), showMIMEType)
import Codec.MIME.Parse (parseMIMEBody, safeParseMIMEBodyByteString, WithoutCRLF(..))
import Text.ParserCombinators.Parsec.Rfc2822 (Field(..), fields)
import Text.ParserCombinators.Parsec (parse)
import EOL (fixCrlfS, fixCrlfB)
import System.Console.GetOpt (OptDescr(..),ArgDescr(..))
import System.IO (Handle, stdout, hPutChar)
import System.IO.Error (ioError, catch, isDoesNotExistError)
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)
import Mbox (Mbox(Mbox), MboxMessage(..), printMbox)
import Data.Digest.Pure.MD5 (md5)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (intersperse)
import Data.Monoid (Monoid(..))

-- read/show extras
mayRead :: Read a => String -> Maybe a
mayRead s = case reads s of
              [(x, "")] -> Just x
              _         -> Nothing

data Email = Email { emailFields  :: [Field]
                   , emailContent :: MIMEValueB
                   , rawEmail     :: B.ByteString
                   }
  deriving (Show)

data ShowFormat = MboxFmt
                | FmtComb FmtComb
  deriving (Eq)

defaultShowFormat :: ShowFormat
defaultShowFormat = oneLinerFmt

data ShowFmt = ShowMessageID -- should stay the first
             | ShowSubject
             | ShowMboxSender
             | ShowMboxTime
             | ShowMD5
             | ShowMIMEType
             | HaskellShow
  deriving (Eq, Enum)

showFmts :: [ShowFmt]
showFmts = [ ShowMessageID .. ]

data FmtComb = Take Int FmtComb
             | Quote FmtComb
             | Const B.ByteString
             | Pure ShowFmt
             | Append FmtComb FmtComb
             | Empty
  deriving (Eq)

instance Monoid FmtComb where
  mempty = Empty
  mappend Empty x = x
  mappend x Empty = x
  mappend x y     = x `Append` y

instance Show ShowFmt where
  show ShowMessageID  = "mid"
  show ShowSubject    = "subj"
  show ShowMboxSender = "mboxsender"
  show ShowMboxTime   = "mboxtime"
  show ShowMD5        = "md5"
  show ShowMIMEType   = "mimetype"
  show HaskellShow    = "show"

mayEvalStr :: String -> Maybe String
mayEvalStr = mayRead . ('\"' :) . foldr escapeDQuote "\""
  where escapeDQuote '"' = ('\\':).('"':)
        escapeDQuote c   = (c:)

mayReadShowFmts :: String -> Maybe FmtComb
mayReadShowFmts = f
  where f ""          = Just mempty
        f ('%':'(':s) = let (s1,s2) = break (==')') s in mappend <$> (Pure <$> lookup s1 fmts) <*> f (drop 1 s2)
        f s           = let (s1,s2) = break (=='%') s in mappend <$> (constC <$> mayEvalStr s1) <*> f s2

        fmts = map (show &&& id) showFmts

mayReadShowFormat :: String -> Maybe ShowFormat -- maintain showFormats*
mayReadShowFormat "mbox" = Just MboxFmt
mayReadShowFormat "from" = Just mboxFrom
mayReadShowFormat "one"  = Just oneLinerFmt
mayReadShowFormat xs     = FmtComb <$> mayReadShowFmts xs

showFormatsDoc :: String
showFormatsDoc = unlines $ ["Message formatting:"
                           ,"  * one : One line per email with: subject, mimetype and message ID (default)"
                           ,"  * mbox: Write emails in mbox format"
                           ,"  * from: One line header of mbox format [as 'From %(mboxsender) %(mboxtime)']"
                           ,"  * Custom:"
                           ,"      fmt  ::= ( '%(' name ')' | string )*"
                           ,"      name ::="] ++
                      map (("             | " ++) . show) showFmts

showFormats :: String
showFormats = "one, mbox, from, <custom>"

constC :: String -> FmtComb
constC = Const . C.pack

mboxFrom :: ShowFormat
mboxFrom = FmtComb $ mconcat [constC "From ", Pure ShowMboxSender, constC " ", Pure ShowMboxTime]

oneLinerFmt :: ShowFormat
oneLinerFmt =
  FmtComb $ mconcat $ intersperse (constC " | ")
          [align 40 $ ellipse 40 $ Pure ShowSubject, align 15 $ Pure ShowMIMEType, Pure ShowMessageID]

align :: Int -> FmtComb -> FmtComb
align n x = Take n (x `mappend` Const (C.repeat ' '))

ellipse :: Int -> FmtComb -> FmtComb
ellipse n s = Take n s `mappend` constC "..."

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

fmtOpt :: (forall err. String -> err) -> (ShowFormat -> a) -> OptDescr a
fmtOpt usage f = Option ['f'] ["fmt"] (ReqArg (f . parseFmt) "FMT") desc
  where parseFmt = maybe (usage "Bad display format") id . mayReadShowFormat
        desc = "Choose the display format (" ++ showFormats ++ ")"

messageId :: Email -> Maybe String
messageId msg = listToMaybe [ mid | MessageID mid <- emailFields msg ]

messageSubject :: Email -> Maybe String
messageSubject msg = listToMaybe [ mid | Subject mid <- emailFields msg ]
{-
  head ([show subject | Subject subject <- flds ]
      ++ ["(malformed subject) " ++ show subject | OptionalField "Subject" subject <- flds ]
      ++ ["no subject, body: " ++ ellipse 40 (show msg)])
-}


unquote :: String -> Maybe String
unquote ('<':xs) = listToMaybe [ys | zs == ">"] where (ys, zs) = break (=='>') xs
unquote _        = Nothing

renderShowFmt :: ShowFmt -> (Email,MboxMessage B.ByteString) -> B.ByteString
renderShowFmt ShowMboxSender = mboxMsgSender . snd
renderShowFmt ShowMboxTime   = mboxMsgTime . snd
renderShowFmt ShowMessageID  = C.pack . fromMaybe "# NO VALID MESSAGE ID" . (>>= unquote) . messageId . fst
renderShowFmt ShowSubject    = C.pack . fromMaybe "# NO VALID SUBJECT" . messageSubject . fst
renderShowFmt ShowMD5        = C.pack . show . md5 . mboxMsgBody . snd
renderShowFmt ShowMIMEType   = C.pack . showMIMEType . mimeType . mime_val_type . emailContent . fst
renderShowFmt HaskellShow    = C.pack . show . fst

renderFmtComb :: FmtComb -> (Email,MboxMessage B.ByteString) -> B.ByteString
renderFmtComb Empty        = const B.empty
renderFmtComb (Pure fmt)   = renderShowFmt fmt
renderFmtComb (Const s)    = const s
renderFmtComb (Take i x)   = B.take (fromIntegral i) . renderFmtComb x
renderFmtComb (Quote x)    = C.pack . show . C.unpack . renderFmtComb x
renderFmtComb (Append x y) = B.append <$> renderFmtComb x <*> renderFmtComb y

hPutB' :: Handle -> B.ByteString -> IO ()
hPutB' h = B.foldlChunks f (return ())
  where f a c = a >> S.hPut h c

putStrLnB' :: B.ByteString -> IO ()
putStrLnB' s = hPutB' stdout s >> hPutChar stdout '\n'

putEmails :: ShowFormat -> [(Email,MboxMessage B.ByteString)] -> IO ()
putEmails MboxFmt       = B.putStr . printMbox . Mbox . map snd
--putEmails (FmtComb fmt) = mapM_ (B.putStrLn . renderFmtComb fmt) -- it's seems to compute a big part (all?) of the list before starting to print (when using mbox-grep for instance)
putEmails (FmtComb fmt) = mapM_ (putStrLnB' . renderFmtComb fmt)

