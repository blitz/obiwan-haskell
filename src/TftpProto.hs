-- Protocol definitions for TFTP v2 (RFC 1350)
-- https://tools.ietf.org/html/rfc1350

-- TODO Blocksize options
-- https://tools.ietf.org/html/rfc2347
-- https://tools.ietf.org/html/rfc2348

module TftpProto (Request(..), RequestError(..), RequestMode(..),
                  decodePacket, encodePacket) where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import           Data.Word             (Word16)

data RequestError = FileNotFound
  | AccessViolation
  | DiskFull
  | IllegalOperation
  | UnknownTransfer
  | FileExists
  | UnknownError !Word16
  deriving (Eq, Show)

data RequestMode = Binary | Ascii
  deriving (Eq, Show)

data Request = RRQ !String !RequestMode
             | WRQ !String !RequestMode
             | DTA !Word16 !B.ByteString
             | ACK !Word16
             | ERR !RequestError !String
  deriving (Eq, Show)

putZeroTermString :: String -> Put
putZeroTermString t = putByteString (BC.pack t) >> putWord8 0

getZeroTermString :: Get String
getZeroTermString = BC.unpack . BL.toStrict <$> getLazyByteStringNul

instance Binary RequestError where
  put FileNotFound     = putWord16be 1
  put AccessViolation  = putWord16be 2
  put DiskFull         = putWord16be 3
  put IllegalOperation = putWord16be 4
  put UnknownTransfer  = putWord16be 5
  put FileExists       = putWord16be 6
  put (UnknownError e) = putWord16be e

  get = do
    errorCode <- getWord16be
    return (case errorCode of
              1 -> FileNotFound
              2 -> AccessViolation
              3 -> DiskFull
              4 -> IllegalOperation
              5 -> UnknownTransfer
              6 -> FileExists
              e -> UnknownError e)

instance Binary RequestMode where
  put Binary = putZeroTermString "octet"
  put Ascii  = putZeroTermString "netascii"

  get =  do
    modeStr <- getZeroTermString
    -- TODO This needs to be a case-insensitive match.
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
    put errCode
    putZeroTermString errMsg

  get = do
    opcode <- getWord16be
    case opcode of
      1 -> RRQ <$> getZeroTermString <*> get
      2 -> WRQ <$> getZeroTermString <*> get
      3 -> DTA <$> getWord16be <*> (BL.toStrict <$> getRemainingLazyByteString)
      4 -> ACK <$> getWord16be
      5 -> ERR <$> get <*> getZeroTermString
      _ -> fail "Unknown opcode"

decodePacket :: B.ByteString -> Maybe Request
decodePacket b = case decodeOrFail (BL.fromStrict b) of
  Right (_, _, r) -> Just r
  Left _          -> Nothing

encodePacket :: Request -> B.ByteString
encodePacket = BL.toStrict . encode
