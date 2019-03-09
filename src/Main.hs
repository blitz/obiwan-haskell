{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad             (forever, void)
import           Data.Binary               (decode, encode, get)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as BL
import           Data.Text.Encoding        (encodeUtf8)
import           Debug.Trace               (traceShow)
import qualified Network.Socket            as S
import qualified Network.Socket.ByteString as SB
import           TftpProto

tftpServerPort :: String
tftpServerPort = "12345"

getReply :: Request -> Maybe Request
getReply (RRQ filename _) = Just $ DTA 1 (encodeUtf8 "Foo!")
getReply (ACK _)          = Nothing
getReply _                = Nothing


parseTftp :: B.ByteString -> Request
parseTftp  = decode . BL.fromStrict

main :: IO ()
main = S.withSocketsDo $ do
  sock <- S.socket S.AF_INET S.Datagram 0
  addrInfo:_ <- S.getAddrInfo (Just (S.defaultHints { S.addrSocketType = S.Datagram}))
              (Just "127.0.0.1") (Just tftpServerPort)
  S.bind sock (S.addrAddress addrInfo)

  forever $ do
    (msg, client) <- SB.recvFrom sock 1500
    case getReply $ parseTftp msg of
      Just reply -> void (SB.sendTo sock (BL.toStrict (encode reply)) client)
      Nothing    -> return ()
