{-# LANGUAGE OverloadedStrings #-}

module TftpProtoSpec (spec) where

import qualified Data.ByteString as B
import           Test.Hspec
import           TftpProto

tftpRrqOctetRequest :: B.ByteString
tftpRrqOctetRequest =
  B.pack [ 0x00, 0x01, 0x73, 0x6f, 0x6d, 0x65, 0x74, 0x68
         , 0x69, 0x6e, 0x67, 0x00, 0x6f, 0x63, 0x74, 0x65
         , 0x74, 0x00 ]

tftpRrqOctetRequestBlksize :: B.ByteString
tftpRrqOctetRequestBlksize =
  tftpRrqOctetRequest
  <> B.pack [ 0x62, 0x6c, 0x6b, 0x73, 0x69, 0x7a, 0x65, 0x00
            , 0x31, 0x30, 0x32, 0x34, 0x00 ]

tftpOackBlksize :: B.ByteString
tftpOackBlksize =
  B.pack [ 0x00, 0x06, 0x62, 0x6c, 0x6b, 0x73, 0x69, 0x7a
         , 0x65, 0x00, 0x31, 0x30, 0x32, 0x34, 0x00 ]

tftpOackBlksizeTooSmall :: B.ByteString
tftpOackBlksizeTooSmall =
  B.pack [ 0x00, 0x06, 0x62, 0x6c, 0x6b, 0x73, 0x69, 0x7a
         , 0x65, 0x00, 0x31, 0x00 ]

tftpRrqOctetRequestBlksizeDenorm :: B.ByteString
tftpRrqOctetRequestBlksizeDenorm =
  tftpRrqOctetRequest
  <> B.pack [ 0x42, 0x6c, 0x6b, 0x73, 0x69, 0x7a, 0x65, 0x00
            , 0x31, 0x30, 0x32, 0x34, 0x00 ]

tftpRrqOctetRequestIncompleteOption :: B.ByteString
tftpRrqOctetRequestIncompleteOption =
  tftpRrqOctetRequest
  <> B.pack [ 0x62, 0x6c, 0x6b, 0x73, 0x69, 0x7a, 0x65, 0x00
           , 0x31, 0x30, 0x32, 0x34 ]

tftpRrqAsciiRequest :: B.ByteString
tftpRrqAsciiRequest =
  B.pack [ 0x00, 0x01, 0x73, 0x6f, 0x6d, 0x65, 0x74, 0x68
         , 0x69, 0x6e, 0x67, 0x00, 0x6e, 0x65, 0x74, 0x61
         , 0x73, 0x63, 0x69, 0x69, 0x00 ]

tftpRrqAsciiCaseDenormRequest :: B.ByteString
tftpRrqAsciiCaseDenormRequest =
  B.pack [ 0x00, 0x01, 0x73, 0x6f, 0x6d, 0x65, 0x74, 0x68
         , 0x69, 0x6e, 0x67, 0x00, 0x6e, 0x45, 0x74, 0x61
         , 0x73, 0x63, 0x69, 0x69, 0x00 ]

tftpRrqOctetInvalidRequest :: B.ByteString
tftpRrqOctetInvalidRequest =
  B.pack [ 0x00, 0x01, 0xc3, 0x28, 0x00, 0x6f, 0x63
         , 0x74, 0x65, 0x74, 0x00 ]

tftpRrqInvalidModeRequest :: B.ByteString
tftpRrqInvalidModeRequest =
  B.pack [ 0x00, 0x01, 0x73, 0x6f, 0x6d, 0x65, 0x74, 0x68
         , 0x69, 0x6e, 0x67, 0x00, 0x12, 0x34, 0x74, 0x65
         , 0x74, 0x00 ]

tftpWrqOctetRequest :: B.ByteString
tftpWrqOctetRequest =
  B.pack [ 0x00, 0x02, 0x73, 0x6f, 0x6d, 0x65, 0x74, 0x68
         , 0x69, 0x6e, 0x67, 0x00, 0x6f, 0x63, 0x74, 0x65
         , 0x74, 0x00 ]

tftpDtaRequest :: B.ByteString
tftpDtaRequest =
  B.pack [ 0x00, 0x03, 0x12, 0x34, 0x73, 0x6f, 0x6d, 0x65
         , 0x74, 0x68, 0x69, 0x6e, 0x67 ]

tftpAckRequest :: B.ByteString
tftpAckRequest =
  B.pack [ 0x00, 0x04, 0x12, 0x34 ]

spec :: Spec
spec = do
  describe "decodePacket" $ do
    it "decodes a RRQ octet packet with an ASCII filename" $
      decodePacket tftpRrqOctetRequest `shouldBe` Just (RRQ "something" Binary [])
    it "decodes a WRQ octet packet with an ASCII filename" $
      decodePacket tftpWrqOctetRequest `shouldBe` Just (WRQ "something" Binary [])
    it "decodes a RRQ netascii packet with an ASCII filename" $
      decodePacket tftpRrqAsciiRequest `shouldBe` Just (RRQ "something" Ascii [])
    it "decodes a DTA packet" $
      decodePacket tftpDtaRequest `shouldBe` Just (DTA 0x1234 "something")
    it "decodes an ACK packet" $
      decodePacket tftpAckRequest `shouldBe` Just (ACK 0x1234)
    it "ignores case in the mode filed while decoding" $
      decodePacket tftpRrqAsciiCaseDenormRequest `shouldBe` Just (RRQ "something" Ascii [])
    -- TFTP is an ASCII only protocol and we should not die on invalid UTF-8.
    it "decodes a RRQ octet packet with invalid UTF-8 filename" $
      decodePacket tftpRrqOctetInvalidRequest `shouldBe` Just (RRQ "\xc3\x28" Binary [])
    it "doesn't decode a RRQ packet with unknown mode" $
      decodePacket tftpRrqInvalidModeRequest `shouldBe` Nothing
    it "doessn't decode a packet with invalid opcode" $
      decodePacket (B.pack [0xFF, 0xFF]) `shouldBe` Nothing
  describe "encodePacket" $ do
    it "encodes a RRQ octet packet with an ASCII filename" $
      encodePacket (RRQ "something" Binary []) `shouldBe` tftpRrqOctetRequest
    it "encodes a WRQ octet packet with an ASCII filename" $
      encodePacket (WRQ "something" Binary []) `shouldBe` tftpWrqOctetRequest
    it "encodes a RRQ netascii packet with an ASCII filename" $
      encodePacket (RRQ "something" Ascii []) `shouldBe` tftpRrqAsciiRequest
    it "encodes a DTA packet" $
      encodePacket (DTA 0x1234 "something") `shouldBe` tftpDtaRequest
    it "encodes an ACK packet" $
      encodePacket (ACK 0x1234) `shouldBe` tftpAckRequest
  describe "decodePacket (options)" $ do
    it "decodes a RRQ octet packet with a blksize option" $
      decodePacket tftpRrqOctetRequestBlksize `shouldBe` Just (RRQ "something" Binary [BlksizeOption 1024])
    it "decodes a OACK packet with a blksize option" $
      decodePacket tftpOackBlksize `shouldBe` Just (OACK [BlksizeOption 1024])
    it "ignores case in option names" $
      decodePacket tftpRrqOctetRequestBlksizeDenorm `shouldBe` Just (RRQ "something" Binary [BlksizeOption 1024])
    it "ignores incomplete options" $
      decodePacket tftpRrqOctetRequestIncompleteOption `shouldBe` Just (RRQ "something" Binary [])
    it "ignores out of bounds blksize options" $
      decodePacket tftpOackBlksizeTooSmall `shouldBe` Just (OACK [])
    -- TODO Add a test that sends an overflowing integer as option value and
    -- require the option (and just this option) to be ignored
  describe "encodePacket (options)" $ do
    it "encodes a RRQ octet packet with a blksize option" $
      encodePacket (RRQ "something" Binary [BlksizeOption 1024]) `shouldBe` tftpRrqOctetRequestBlksize
    it "encodes a OACK packet with a blksize option" $
      encodePacket (OACK [BlksizeOption 1024]) `shouldBe` tftpOackBlksize
