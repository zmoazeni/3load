module Conversions (
  byteStringToLazy
  ,byteStringFromLazy
  ,stringToByteString
  ,byteStringToString
  ,encode'
  ,decode'
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Binary

byteStringToLazy :: B.ByteString -> BL.ByteString
byteStringToLazy = BL.fromChunks . (:[])

byteStringFromLazy :: BL.ByteString -> B.ByteString
byteStringFromLazy = B.concat . BL.toChunks

stringToByteString :: String -> B.ByteString
stringToByteString = encodeUtf8 . pack

byteStringToString :: B.ByteString -> String
byteStringToString = unpack . decodeUtf8

encode' :: (Binary a) => a -> B.ByteString
encode' = byteStringFromLazy . encode

decode' :: (Binary a) => B.ByteString -> a
decode' = decode . byteStringToLazy

