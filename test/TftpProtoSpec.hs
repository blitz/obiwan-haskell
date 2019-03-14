{-# LANGUAGE OverloadedStrings #-}

module TftpProtoSpec (spec) where

import qualified Data.ByteString as B
import           Test.Hspec
import           TftpProto

tftpRrqOctetRequest :: B.ByteString
tftpRrqOctetRequest = B.pack [0x00, 0x01, 0x73, 0x6f, 0x6d, 0x65, 0x74, 0x68,
                              0x69, 0x6e, 0x67, 0x00, 0x6f, 0x63, 0x74, 0x65,
                              0x74, 0x00]

tftpRrqOctetInvalidRequest :: B.ByteString
tftpRrqOctetInvalidRequest = B.pack [0x00, 0x01, 0xc3, 0x28, 0x00, 0x6f, 0x63,
                                     0x74, 0x65, 0x74, 0x00]

spec :: Spec
spec = do
  describe "decodePacket" $ do
    it "decodes a RRQ octet packet with an ASCII filename" $
      decodePacket tftpRrqOctetRequest `shouldBe` Just (RRQ "something" Binary)
    -- TFTP is an ASCII only protocol and we should not die on invalid UTF-8.
    it "decodes a RRQ octet packet with invalid UTF-8 filename" $
      decodePacket tftpRrqOctetInvalidRequest `shouldBe` Just (RRQ "\xc3\x28" Binary)
