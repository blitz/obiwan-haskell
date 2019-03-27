{-# LANGUAGE OverloadedStrings #-}

-- Protocol definitions for TFTP v2 (RFC 1350)
-- https://tools.ietf.org/html/rfc1350

-- https://tools.ietf.org/html/rfc2347
-- https://tools.ietf.org/html/rfc2348
-- https://tools.ietf.org/html/rfc2349

module TftpProto
  ( Request(..)
  , RequestError(..)
  , RequestMode(..)
  , RequestOption(..)
  , decodePacket
  , encodePacket
  ) where

import           Control.Applicative   (many)
import           Control.Monad         (guard)
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import qualified Data.CaseInsensitive  as CI
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

data RequestOption = BlksizeOption Int       -- RFC2348
            -- | Timeout Int       -- RFC2349
            -- | TransferSize Int  -- RFC2349
            | UnknownOption String String
  deriving (Eq, Show)

data Request = RRQ !String !RequestMode ![RequestOption]
             | WRQ !String !RequestMode ![RequestOption]
             | DTA !Word16 !B.ByteString
             | ACK !Word16
             | ERR !RequestError !String
             | OACK ![RequestOption]
  deriving (Eq, Show)

putZeroTermString :: String -> Put
putZeroTermString t = putByteString (BC.pack t) >> putWord8 0

getZeroTermString :: Get String
getZeroTermString = BC.unpack . BL.toStrict <$> getLazyByteStringNul

-- TODO This is b0rken, because "read" will silently generate garbage integers
-- when Int overflows.
toInt :: String -> Maybe Int
toInt = Just . read

inBounds :: Int -> Int -> Int -> Maybe Int
inBounds loBound hiBound v = guard ((v >= loBound) && (v <= hiBound)) >> return v

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
    case CI.mk modeStr of
      "netascii" -> return Ascii
      "octet"    -> return Binary
      _          -> fail "Unknown mode"

instance Binary RequestOption where
  put (BlksizeOption b) = do
    putZeroTermString "blksize"
    putZeroTermString (show b)

  put (UnknownOption k v) = do
    putZeroTermString k
    putZeroTermString v

  get = do
    optionName <- getZeroTermString
    optionValue <- getZeroTermString
    case CI.mk optionName of
      "blksize" -> maybe (fail "Integer out of bounds") (pure . BlksizeOption) blksizeValue
        where blksizeValue = toInt optionValue >>= inBounds 8 65464
      _         -> pure $ UnknownOption optionName optionValue

-- TODO This ignores everything after options that trigger a failure case, i.e.
-- any option after a blksize option with length 0 would be ignored.
getOptionList :: Get [RequestOption]
getOptionList = many get

putOptionList :: [RequestOption] -> Put
putOptionList = mapM_ put

instance Binary Request where
  put (RRQ filename mode options) = do
    putWord16be 1
    putZeroTermString filename
    put mode
    putOptionList options

  put (WRQ filename mode options) = do
    putWord16be 2
    putZeroTermString filename
    put mode
    putOptionList options

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

  put (OACK options) = do
    putWord16be 6
    putOptionList options

  get = do
    opcode <- getWord16be
    case opcode of
      1 -> RRQ <$> getZeroTermString <*> get <*> getOptionList
      2 -> WRQ <$> getZeroTermString <*> get <*> getOptionList
      3 -> DTA <$> getWord16be <*> (BL.toStrict <$> getRemainingLazyByteString)
      4 -> ACK <$> getWord16be
      5 -> ERR <$> get <*> getZeroTermString
      6 -> OACK <$> getOptionList
      _ -> fail "Unknown opcode"

decodePacket :: B.ByteString -> Maybe Request
decodePacket b = case decodeOrFail (BL.fromStrict b) of
  Right (_, _, r) -> Just r
  Left _          -> Nothing

encodePacket :: Request -> B.ByteString
encodePacket = BL.toStrict . encode
