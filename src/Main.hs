module Main where

import qualified Network.Socket            as S
import qualified Network.Socket.ByteString as SB

tftpServerPort :: S.PortNumber
tftpServerPort = 12345

main :: IO ()
main = S.withSocketsDo $ do
  sock <- S.socket S.AF_INET S.Datagram 0
  addrInfo:_ <- S.getAddrInfo (Just (S.defaultHints { S.addrSocketType = S.Datagram}))
              (Just "127.0.0.1") (Just "12345")
  S.bind sock (S.addrAddress addrInfo)

  loop sock
  where
    loop sock = do
      (mesg, client) <- SB.recvFrom sock 1500
      -- TODO Do something with the message
      _ <- SB.sendTo sock mesg client
      loop sock
