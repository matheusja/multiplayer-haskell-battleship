module Code where
import Data.ByteString
import Conversions
import Data.IORef
newtype Type = Type Word deriving (Eq, Ord);


code_text :: Type -> String
code_text = show . unwrap

encode :: Type -> ByteString
encode = string_to_utf8 . code_text

decode :: ByteString -> Type
decode = Type . read . string_from_utf8

unwrap :: Type -> Word
unwrap(Type c) = c