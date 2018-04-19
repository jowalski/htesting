module Shrinking where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Codec.Binary.Base64 as CB (encode, decode)
import GHC.Int

-- assuming our encode function is defined
-- encode :: BL.ByteString -> BL.ByteString
encode :: BL.ByteString -> BL.ByteString
encode = BL.fromStrict . CB.encode . BL.toStrict

sizes :: String -> [GHC.Int.Int64]
sizes s = [(BL.length . encode . pack) s,(BL.length . pack) s]

pack :: String -> BL.ByteString
pack = BL.fromStrict . BC.pack

-- Getting back to the Base64 example, we want to enforce that encode and decode
-- are inverses.  Encoding first and then decoding is the easy direction because
-- encode is not picky about its inputs. The propery looks like this:
-- assume the decode function has this type:
-- decode :: ByteString -> Either ParseError ByteString
decode
  :: BL.ByteString -> Either String BL.ByteString
decode b =
  case d of
    Left (_,x) -> Left "error"
    Right y -> (Right . BL.fromStrict) y
  where d = (CB.decode . BL.toStrict) b