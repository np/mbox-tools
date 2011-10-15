--------------------------------------------------------------------
-- |
-- Module    : FmtComb
-- Copyright : (c) Nicolas Pouillard 2010, 2011
-- License   : BSD3
--
-- Maintainer: Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

{-# LANGUAGE BangPatterns,
             OverloadedStrings, GeneralizedNewtypeDeriving #-}
module FmtComb where

import Control.Applicative
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
{- TMP-NO-MIME
import Codec.MIME.Type (MIMEValue(..), Type(..), showMIMEType)
-}
import Codec.Mbox (MboxMessage(..), showMboxMessage)
import qualified Data.Digest.Pure.MD5 as MD5 (md5)
import Data.Maybe (fromMaybe)
import Data.List (intersperse)
import Data.Monoid (Monoid(..))
import Data.Char (isSpace)
import Email

type Message = (Email, MboxMessage B.ByteString)

newtype ReaderMsg a = ReaderMsg { runReaderMsg :: Reader Message a }
  deriving (Monad, Functor, Applicative, MonadReader Message)

type FmtComb = ReaderMsg B.ByteString
type FmtMod = FmtComb -> FmtComb

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

showC ::Show a => a -> B.ByteString
showC = C.pack . show

viaS :: (S.ByteString -> S.ByteString) -> B.ByteString -> B.ByteString
viaS f = C.fromChunks . pure . f . foldr S.append S.empty . C.toChunks

stripS ::S.ByteString -> S.ByteString
stripS = S8.dropWhile isSpace . fst . S8.spanEnd isSpace

stripC ::B.ByteString -> B.ByteString
stripC = viaS stripS

strip ::String -> String
strip = S8.unpack . stripS . S8.pack

split :: Char -> String -> [String]
split c = map S8.unpack . S8.split c . S8.pack

mkF :: (MboxMessage B.ByteString -> a) -> ReaderMsg a
mkF f = asks (f . snd)

mboxMsgSenderF, mboxMsgTimeF, mboxFromF, mboxMsgFileF, mboxMsgOffsetF,
  mboxMsgF, mboxMsgBodyF, messageIDF, subjectF, {-TMP-NO-MIME mimeTypeF,-} emailShown,
  oneLinerF :: FmtComb

mboxMsgTimeF   = mkF _mboxMsgTime
mboxMsgSenderF = mkF _mboxMsgSender
mboxMsgFileF   = mkF $ C.pack . _mboxMsgFile
mboxMsgOffsetF = mkF $ showC . _mboxMsgOffset
mboxMsgF       = mkF $ flip C.snoc '\n' . showMboxMessage
mboxMsgBodyF   = mkF _mboxMsgBody
emailShown     = showC . fst <$> ask
mboxFromF
  = mconcat [pure "From ", mboxMsgSenderF, pure " ", mboxMsgTimeF]
messageIDF
  = C.pack . fromMaybe "<NO-VALID-MESSAGE-ID>"
           . (>>= unquote) . messageId . fst <$> ask
subjectF
  = C.pack . fromMaybe "<NO-VALID-SUBJECT>" . messageSubject . fst <$> ask
{- TMP-NO-MIME
mimeTypeF
  = C.pack . showMIMEType . mimeType . mime_val_type
           . get emailContent . fst <$> ask
-}
oneLinerF
  = mconcat $ intersperse (pure " | ")
       [ align 40 $ ellipse 40 $ subjectF
       , {- TMP-NO-MIME align 15 $ mimeTypeF,-} messageIDF ]

align :: Int -> FmtMod
align n x = B.take (fi n) <$> (x `mappend` pure (C.repeat ' '))

ellipse :: Int -> FmtMod
ellipse n s = (B.take (fi n) <$> s) `mappend` pure "..."

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

size, md5 :: FmtMod
size = fmap (showC . C.length)
md5  = fmap (showC . MD5.md5)

fmtCombs :: [(String, (FmtComb, String))]
fmtCombs = [ ("one",           (oneLinerF      , "One line per email with: subject, mimetype and message ID (default)"))
           , ("subj",          (subjectF       , "Subject"))
           , ("mboxmsgsender", (mboxMsgSenderF , "Mbox Sender"))
           , ("mboxmsgtime",   (mboxMsgTimeF   , "Mbox Msg Time"))
           , ("mboxmsgfile",   (mboxMsgFileF   , "Mbox Msg File"))
           , ("offset",        (mboxMsgOffsetF , "Mbox Offset"))
           , ("mboxmsg",       (mboxMsgF       , "Mbox Msg"))
           , ("mboxmsgbody",   (mboxMsgBodyF   , "Mbox Msg Body"))
           -- TMP-NO-MIME , ("mimetype",      (mimeTypeF      , "MIME type"))
           , ("fromline",      (mboxFromF      , "Mbox From Line [as 'From %(mboxmsgsender) %(mboxmsgtime)']"))
           , ("mid",           (messageIDF     , "Message ID"))
           ]

fmtMods :: [(String, (FmtMod, String))]
fmtMods = [ ("size",  (size, "Commpute the size of the input"))
          , ("md5",   (md5,  "Hash the input with MD5"))
          , ("strip", (fmap stripC, "Strip leading and trailling spaces"))
          ]

intFmtMods :: [(String, ((Int -> FmtMod), String))]
intFmtMods = [ ("ellipse",  (ellipse, "Truncate at N and put an ellipse"))
             , ("align",    (align,   "Fill upto N with spaces"))
             , ("take",     (fmap . C.take . fi, "Take the N firsts"))
             , ("drop",     (fmap . C.take . fi, "Drop the N firsts"))
             ]

mayEvalStr :: String -> Maybe String
mayEvalStr = mayRead . ('\"' :) . foldr escapeDQuote "\""
  where escapeDQuote '"' = ('\\':).('"':)
        escapeDQuote c   = (c:)

mayReadIntFmtMod :: String -> Maybe FmtMod
mayReadIntFmtMod s = fst <$> lookup s1 intFmtMods <*> mayRead s2
  where (s1, s2) = break isSpace s

mayReadFmtMod :: String -> Maybe FmtMod
mayReadFmtMod s = (fst <$> lookup s fmtMods) `mplus` mayReadIntFmtMod s

mayReadFmtComb :: String -> Maybe FmtComb
mayReadFmtComb s = case reverse (split '.' s) of
  []       -> Nothing
  lst : xs -> do cmb  <- fst <$> lookup lst fmtCombs
                 mods <- mapM (mayReadFmtMod . strip) (reverse xs)
                 return $ foldr (.) id mods cmb

mayReadShowFmts :: String -> Maybe FmtComb
mayReadShowFmts = f
  where f []          = Just mempty
        f ('%':'(':s) = let (s1,s2) = break (==')') s in
                        mappend <$> (mayReadFmtComb s1) <*> f (drop 1 s2)
        f s           = let (s1,s2) = break (=='%') s in
                        mappend <$> (pure . C.pack <$> mayEvalStr s1) <*> f s2

renderFmtComb :: FmtComb -> Message -> B.ByteString
renderFmtComb = flip evalReaderMsg
