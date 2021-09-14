module Lobbying(Code(), code_text, encode, decode, alocator) where 
import Data.ByteString(ByteString)
import Conversions(string_to_utf8, string_from_utf8)
import Data.IORef
newtype Code = Code Word;


code_text :: Code -> String
code_text = show . unwrap

encode :: Code -> ByteString
encode = string_to_utf8 . code_text

decode :: ByteString -> Code
decode = Code . read . string_from_utf8

unwrap :: Code -> Word
unwrap(Code c) = c

alocator :: IO (IO Code)
alocator = do
  counter_now <- newIORef 0;
  return $ do
    counter <- readIORef counter_now
    atomicModifyIORef' counter_now (\x -> (x+1,x))
    return $ Code counter 

