module TftpProto (Request(..), decodePacket, encodePacket) where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Text            (Text)
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import           Data.Word            (Word16)

data Request = RRQ !Text !Text
             | WRQ !Text !Text
             | DTA !Word16 !B.ByteString
             | ACK !Word16
             | ERR !Word16 !Text
  deriving (Show)

putZeroTermString :: Text -> Put
putZeroTermString t = putByteString (encodeUtf8 t) >> putWord8 0

getZeroTermString :: Get Text
getZeroTermString = decodeUtf8 . BL.toStrict <$> getLazyByteStringNul

instance Binary Request where
  put (RRQ filename mode) = do
    putWord16be 1
    putZeroTermString filename
    putZeroTermString mode

  put (WRQ filename mode) = do
    putWord16be 2
    putZeroTermString filename
    putZeroTermString mode

  put (DTA block fData) = do
    putWord16be 3
    putWord16be block
    putByteString fData

  put (ACK block) = do
    putWord16be 4
    putWord16be block

  put (ERR errCode errMsg) = do
    putWord16be 5
    putWord16be errCode
    putZeroTermString errMsg

  get = do
    opcode <- getWord16be
    case opcode of
      1 -> RRQ <$> getZeroTermString <*> getZeroTermString
      2 -> WRQ <$> getZeroTermString <*> getZeroTermString
      3 -> DTA <$> getWord16be <*> (BL.toStrict <$> getRemainingLazyByteString)
      4 -> ACK <$> getWord16be
      5 -> ERR <$> getWord16be <*> getZeroTermString
      _ -> fail "Unknown opcode"

decodePacket :: B.ByteString -> Maybe Request
decodePacket b = case decodeOrFail (BL.fromStrict b) of
  Right (_, _, r) -> Just r
  Left _          -> Nothing

encodePacket :: Request -> B.ByteString
encodePacket = BL.toStrict . encode
