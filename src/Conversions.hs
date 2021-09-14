module Conversions where
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.ByteString.Char8 as C

string_to_utf8 :: String -> C.ByteString
string_to_utf8 = DTE.encodeUtf8 . DT.pack

string_from_utf8 :: C.ByteString -> String
string_from_utf8 = DT.unpack . DTE.decodeUtf8


