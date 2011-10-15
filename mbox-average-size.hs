import Codec.Mbox (Mbox(..), MboxMessage(..), parseMbox, mboxMsgBody)
import Control.Applicative ((<$>))
import Data.List (foldl')
import Data.Label
import qualified Data.ByteString.Lazy as B

data P a = P !a !Int

-- spec: average xs = sum xs / fromIntegral (length xs)
-- still slow (too lazy tuple): average = uncurry (/) . foldl' f (0,0) where f (a,b) x = (a+x,b+1)
average :: Fractional a => [a] -> a
average xs = s / fromIntegral l
  where (P s l)     = foldl' f (P 0 0) xs
        f (P a b) x = P (a + x) (b + 1)

mboxAverageSize :: Mbox B.ByteString -> Double
mboxAverageSize = average . map (fromIntegral . B.length . get mboxMsgBody) . mboxMessages

main :: IO ()
main = do putStrLn "Reading from stdin..."
          print =<< (mboxAverageSize . parseMbox) <$> B.getContents
