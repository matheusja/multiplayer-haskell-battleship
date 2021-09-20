module Conversions where
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List (foldl')

string_to_utf8 :: String -> C.ByteString
string_to_utf8 = DTE.encodeUtf8 . DT.pack

string_from_utf8 :: C.ByteString -> String
string_from_utf8 = DT.unpack . DTE.decodeUtf8


parseChar :: Char -> Maybe Int
parseChar x | x `elem` ['0'..'9'] = Just $ ord x - ord '0'
parseChar _ = Nothing

parseInt :: String -> Maybe Integer
parseInt = fmap (foldl' (\acc v -> acc * 10 + v) 0) . mapM (fmap fromIntegral . parseChar)