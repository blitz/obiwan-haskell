{-# LANGUAGE OverloadedStrings #-}

module TftpProto (Request(..), RequestMode(..), decodePacket, encodePacket) where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Text            (Text)
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import           Data.Word            (Word16)

data RequestMode = Binary | Ascii
  deriving (Eq, Show)

data Request = RRQ !Text !RequestMode
             | WRQ !Text !RequestMode
             | DTA !Word16 !B.ByteString
             | ACK !Word16
             | ERR !Word16 !Text
  deriving (Eq, Show)

putZeroTermString :: Text -> Put
putZeroTermString t = putByteString (encodeUtf8 t) >> putWord8 0

getZeroTermString :: Get Text
getZeroTermString = decodeUtf8 . BL.toStrict <$> getLazyByteStringNul

instance Binary RequestMode where
  put Binary = putZeroTermString "octet"
  put Ascii  = putZeroTermString "netascii"

  get =  do
    modeStr <- getZeroTermString
    case modeStr of
      "netascii" -> return Ascii
      "octet"    -> return Binary
      _          -> fail "Unknown mode"

instance Binary Request where
  put (RRQ filename mode) = do
    putWord16be 1
    putZeroTermString filename
    put mode

  put (WRQ filename mode) = do
    putWord16be 2
    putZeroTermString filename
    put mode

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
      1 -> RRQ <$> getZeroTermString <*> get
      2 -> WRQ <$> getZeroTermString <*> get
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
