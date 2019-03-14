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
      decodePacket tftpRrqOctetRequest `shouldBe` Just (RRQ "something" Binary)
    it "decodes a WRQ octet packet with an ASCII filename" $
      decodePacket tftpWrqOctetRequest `shouldBe` Just (WRQ "something" Binary)
    it "decodes a RRQ netascii packet with an ASCII filename" $
      decodePacket tftpRrqAsciiRequest `shouldBe` Just (RRQ "something" Ascii)
    it "decodes a DTA packet" $
      decodePacket tftpDtaRequest `shouldBe` Just (DTA 0x1234 "something")
    it "decodes an ACK packet" $
      decodePacket tftpAckRequest `shouldBe` Just (ACK 0x1234)
    it "ignores case in the mode filed while decoding" $
      decodePacket tftpRrqAsciiCaseDenormRequest `shouldBe` Just (RRQ "something" Ascii)
    -- TFTP is an ASCII only protocol and we should not die on invalid UTF-8.
    it "decodes a RRQ octet packet with invalid UTF-8 filename" $
      decodePacket tftpRrqOctetInvalidRequest `shouldBe` Just (RRQ "\xc3\x28" Binary)
    it "doesn't decode a RRQ packet with unknown mode" $
      decodePacket tftpRrqInvalidModeRequest `shouldBe` Nothing
    it "doessn't decode a packet with invalid opcode" $
      decodePacket (B.pack [0xFF, 0xFF]) `shouldBe` Nothing
  describe "encodePacket" $ do
    it "encodes a RRQ octet packet with an ASCII filename" $
      encodePacket (RRQ "something" Binary) `shouldBe` tftpRrqOctetRequest
    it "encodes a WRQ octet packet with an ASCII filename" $
      encodePacket (WRQ "something" Binary) `shouldBe` tftpWrqOctetRequest
    it "encodes a RRQ netascii packet with an ASCII filename" $
      encodePacket (RRQ "something" Ascii) `shouldBe` tftpRrqAsciiRequest
    it "encodes a DTA packet" $
      encodePacket (DTA 0x1234 "something") `shouldBe` tftpDtaRequest
    it "encodes an ACK packet" $
      encodePacket (ACK 0x1234) `shouldBe` tftpAckRequest
