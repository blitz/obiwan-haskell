{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad             (forever)
import           Data.Functor              ((<&>))
import           Data.Text.Encoding        (encodeUtf8)
import qualified Network.Socket            as S
import qualified Network.Socket.ByteString as SB
import           TftpProto

tftpServerPort :: String
tftpServerPort = "12345"

getReply :: Request -> Maybe Request
getReply (RRQ filename _) = Just $ DTA 1 (encodeUtf8 "Foo!")
getReply (ACK _)          = Nothing
getReply _                = Nothing

maybeDo :: Maybe a -> (a -> IO ()) -> IO ()
maybeDo m f = maybe (return ()) f m

main :: IO ()
main = S.withSocketsDo $ do
  sock <- S.socket S.AF_INET S.Datagram 0
  addrInfo:_ <- S.getAddrInfo (Just (S.defaultHints { S.addrSocketType = S.Datagram}))
              (Just "127.0.0.1") (Just tftpServerPort)
  S.bind sock (S.addrAddress addrInfo)

  forever $ do
    (msg, client) <- SB.recvFrom sock 1500
    (decodePacket msg >>= getReply <&> encodePacket) `maybeDo` (\buf -> SB.sendAllTo sock buf client)
