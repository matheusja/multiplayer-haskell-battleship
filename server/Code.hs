module Code where
import Data.ByteString
import Data.IORef
import Conversions
newtype Type = Type Integer deriving (Eq, Ord);

{-# ANN module "HLint: ignore Use camelCase" #-}

code_text :: Type -> String
code_text = show . unwrap

encode :: Type -> ByteString
encode = string_to_utf8 . code_text

decode :: String -> Maybe Type
decode = fmap Type . parseInt

unwrap :: Type -> Integer
unwrap(Type c) = c