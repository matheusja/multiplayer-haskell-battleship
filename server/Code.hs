module Code(Type(), code_text, encode, decode, unwrap, alocator) where
import Data.ByteString
import Conversions
import Data.IORef
newtype Type = Type Word;


code_text :: Type -> String
code_text = show . unwrap

encode :: Type -> ByteString
encode = string_to_utf8 . code_text

decode :: ByteString -> Type
decode = Type . read . string_from_utf8

unwrap :: Type -> Word
unwrap(Type c) = c

alocator :: IO (IO Code.Type)
alocator = do
  counter_now <- newIORef 0;
  return $ Code.Type <$> atomicModifyIORef' counter_now (\x -> (x+1,x))