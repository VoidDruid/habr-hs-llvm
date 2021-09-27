module StringUtils where

import Data.List

-- String stuff
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString, toShort)

show' :: Show a => a -> ByteString
show' = BSU.fromString . show

showShort :: Show a => a -> ShortByteString
showShort = toShort . show'

toShort' :: String -> ShortByteString
toShort' = toShort . BSU.fromString

addToLast :: [String] -> String -> [String]
addToLast [s] str = [s ++ str]
addToLast (reverse -> s:ss) str = reverse ss ++ [s ++ str]

joinS = unwords
joinN = intercalate "\n"
joinC = intercalate ", "
