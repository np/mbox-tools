--------------------------------------------------------------------
-- |
-- Module    : Email
-- Copyright : (c) Nicolas Pouillard 2008, 2009
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

{-# LANGUAGE BangPatterns, Rank2Types, TemplateHaskell,
             OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Email where

import Control.Applicative
import Control.Arrow
import Control.Monad.Reader
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
import Codec.Mbox (Mbox(Mbox), MboxMessage(..), showMbox, showMboxMessage)
import Data.Digest.Pure.MD5 (md5)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (intersperse)
import Data.Monoid (Monoid(..))
import Data.Char (toLower)
import Data.Int (Int64)
import Data.Accessor.Template

type Message = (Email, MboxMessage B.ByteString)

newtype ReaderMsg a = ReaderMsg { runReaderMsg :: Reader Message a }
  deriving (Monad, Functor, Applicative, MonadReader Message)

type FmtComb = ReaderMsg B.ByteString

instance Monoid w => Monoid (ReaderMsg w) where
  mempty = pure mempty
  mappend x y = mappend <$> x <*> y

evalReaderMsg :: Message -> ReaderMsg a -> a
evalReaderMsg msg = flip runReader msg . runReaderMsg

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
$(nameDeriveAccessors ''Email $ Just.(++ "A"))

data ShowFormat = MboxFmt
                | FmtComb FmtComb

defaultShowFormat :: ShowFormat
defaultShowFormat = oneLinerFmt

data ShowFmt = ShowMessageID -- should stay the first
             | ShowSubject
             | ShowMboxMsgSender
             | ShowMboxMsgTime
             | ShowMboxMsgFile
             | ShowMboxMsgOffset
             | ShowMboxSize
             | ShowMboxBodySize
             | ShowMboxMsgBodyMD5
             | ShowMIMEType
             | HaskellShow
  deriving (Eq, Enum)

showFmts :: [ShowFmt]
showFmts = [ ShowMessageID .. ]

instance Show ShowFmt where
  show ShowMessageID      = "mid"
  show ShowSubject        = "subj"
  show ShowMboxMsgSender  = "mboxmsgsender"
  show ShowMboxMsgTime    = "mboxmsgtime"
  show ShowMboxMsgFile    = "mboxmsgfile"
  show ShowMboxMsgOffset  = "offset"
  show ShowMboxSize       = "mboxmsgsize"
  show ShowMboxBodySize   = "mboxmsgbodysize"
  show ShowMboxMsgBodyMD5 = "mboxmsgbodymd5"
  show ShowMIMEType       = "mimetype"
  show HaskellShow        = "haskellshow"

mayEvalStr :: String -> Maybe String
mayEvalStr = mayRead . ('\"' :) . foldr escapeDQuote "\""
  where escapeDQuote '"' = ('\\':).('"':)
        escapeDQuote c   = (c:)

mayReadShowFmts :: String -> Maybe FmtComb
mayReadShowFmts = f
  where f []          = Just mempty
        f ('%':'(':s) = let (s1,s2) = break (==')') s in mappend <$> (showFmt <$> lookup s1 fmts) <*> f (drop 1 s2)
        f s           = let (s1,s2) = break (=='%') s in mappend <$> (text . C.pack <$> mayEvalStr s1) <*> f s2

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
                           ,"  * from: One line header of mbox format [as 'From %(mboxmsgsender) %(mboxmsgtime)']"
                           ,"  * Custom:"
                           ,"      fmt  ::= ( '%(' name ')' | string )*"
                           ,"      name ::="] ++
                      map (("             | " ++) . show) showFmts

showFormats :: String
showFormats = "one, mbox, from, <custom>"

showFmt :: ShowFmt -> FmtComb
showFmt fmt = renderShowFmt fmt <$> ask

text :: B.ByteString -> FmtComb
text = pure

mboxFrom :: ShowFormat
mboxFrom = FmtComb $ mconcat [text "From ", showFmt ShowMboxMsgSender, text " ", showFmt ShowMboxMsgTime]

oneLinerFmt :: ShowFormat
oneLinerFmt =
  FmtComb $ mconcat $ intersperse (text " | ")
          [align 40 $ ellipse 40 $ showFmt ShowSubject, align 15 $ showFmt ShowMIMEType, showFmt ShowMessageID]

align :: Int64 -> FmtComb -> FmtComb
align n x = B.take n <$> (x `mappend` text (C.repeat ' '))

ellipse :: Int64 -> FmtComb -> FmtComb
ellipse n s = B.take n <$> s `mappend` text "..."

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
          Email { emailFields = headers
                --, emailContent = parseMIMEBody optional_headers (fixCrlfB body)
                --, emailContent = fmap C.pack $ parseMIMEBody optional_headers (fixCrlfS (C.unpack body))
                --, emailContent = safeParseMIMEBodyByteString optional_headers (fixCrlfB body)
                , emailContent = dynParseMIMEBody optional_headers body
                , rawEmail = orig }
          where headers = readFields flds
                optional_headers = [ (k,v) | OptionalField k v <- headers ]

readMboxEmails :: Mbox B.ByteString -> [Email]
readMboxEmails = map (readEmail . mboxMsgBody) . mboxMessages

fmtOpt :: (forall err. String -> err) -> (ShowFormat -> a) -> OptDescr a
fmtOpt usage f = Option "f" ["fmt"] (ReqArg (f . parseFmt) "FMT") desc
  where parseFmt = fromMaybe (usage "Bad display format") . mayReadShowFormat
        desc = "Choose the display format (" ++ showFormats ++ ")"

stringOfField :: Field -> (String, String)
stringOfField (MessageID x) = ("message-id", fromMaybe (error "impossible: Email.stringOfField") $ unquote x)
stringOfField (Subject   x) = ("subject",    x)
stringOfField (OptionalField x y) = (map toLower x, y)
stringOfField x = ("x-unknown", show x) -- TODO

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

renderShowFmt :: ShowFmt -> Message -> B.ByteString
renderShowFmt ShowMboxMsgSender  = mboxMsgSender . snd
renderShowFmt ShowMboxMsgTime    = mboxMsgTime . snd
renderShowFmt ShowMboxMsgFile    = C.pack . mboxMsgFile . snd
renderShowFmt ShowMboxMsgOffset  = C.pack . show . mboxMsgOffset . snd
renderShowFmt ShowMboxSize       = C.pack . show . (+1) . C.length . showMboxMessage . snd
renderShowFmt ShowMboxBodySize   = C.pack . show . C.length . mboxMsgBody . snd
renderShowFmt ShowMessageID      = C.pack . fromMaybe "<NO-VALID-MESSAGE-ID>" . (>>= unquote) . messageId . fst
renderShowFmt ShowSubject        = C.pack . fromMaybe "<NO-VALID-SUBJECT>" . messageSubject . fst
renderShowFmt ShowMboxMsgBodyMD5 = C.pack . show . md5 . mboxMsgBody . snd
renderShowFmt ShowMIMEType       = C.pack . showMIMEType . mimeType . mime_val_type . emailContent . fst
renderShowFmt HaskellShow        = C.pack . show . fst

renderFmtComb :: FmtComb -> Message -> B.ByteString
renderFmtComb = flip evalReaderMsg

hPutB' :: Handle -> B.ByteString -> IO ()
hPutB' h = go
  where go B.Empty        = return ()
        go (B.Chunk c cs) = S.hPut h c >> go cs

putStrLnB' :: B.ByteString -> IO ()
putStrLnB' s = hPutB' stdout s >> hPutChar stdout '\n'

putEmails :: ShowFormat -> [(Email,MboxMessage B.ByteString)] -> IO ()
putEmails MboxFmt       = B.putStr . showMbox . Mbox . map snd
--putEmails (FmtComb fmt) = mapM_ (B.putStrLn . renderFmtComb fmt) -- it's seems to compute a big part (all?) of the list before starting to print (when using mbox-grep for instance)
putEmails (FmtComb fmt) = mapM_ (putStrLnB' . renderFmtComb fmt)

