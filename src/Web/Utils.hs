module Web.Utils where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16

makeHex :: BS.ByteString -> T.Text
makeHex = T.decodeUtf8 . B16.encode

decodeHex :: T.Text -> BS.ByteString
decodeHex = fst . B16.decode . T.encodeUtf8
