{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module TftpServer (serveTftp) where

import           Control.Concurrent        (forkIO)
import           Control.Monad             (void)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader      (ReaderT, ask, runReaderT)
import           Data.Functor              ((<&>))
import qualified Network.Socket            as S
import qualified Network.Socket.ByteString as SB
import           System.Timeout            (timeout)

import           TftpConnection
import           TftpContent               (Content)

-- Constants

tftpMaxPacketSize :: Int
tftpMaxPacketSize = 65536       -- Largest possible IP packet

-- Type definitions and interface implementation

data Client = Client { clientAddress :: S.SockAddr
                     , clientSocket  :: S.Socket
                     }

instance MonadIO m => MonadTftpConnection (ReaderT Client m) where
  recvData timeoutMs = do
    client <- ask
    liftIO $ timeout timeoutMs $ SB.recvFrom (clientSocket client) tftpMaxPacketSize <&> fst

  sendData buf = do
    client <- ask
    liftIO $ SB.sendAll (clientSocket client) buf

  logMsg msg = do
    client <- ask
    liftIO $ putStrLn $ show (clientAddress client) ++ " | " ++ msg

-- Network helpers

createBoundUdpSocket :: String -> String -> IO S.Socket
createBoundUdpSocket address service = do
  sock <- S.socket S.AF_INET S.Datagram 0
  addrInfo:_ <-
    S.getAddrInfo
      (Just (S.defaultHints {S.addrSocketType = S.Datagram}))
      (Just address)
      (Just service)
  S.bind sock (S.addrAddress addrInfo)
  return sock

createConnectedUdpSocket :: S.SockAddr -> IO Client
createConnectedUdpSocket sockaddr = do
  sock <- S.socket S.AF_INET S.Datagram 0
  S.connect sock sockaddr
  return $ Client sockaddr sock

-- Main server loop

loopForever :: a -> (a -> IO a) -> IO ()
loopForever s f = void (loopForever_ s)
  where loopForever_ s_ = f s_ >>= (`loopForever` f)

serveTftp :: String -> String -> Content -> IO ()
serveTftp address service content = S.withSocketsDo $ do
  serverSocket <- createBoundUdpSocket address service
  putStrLn $ "Listening on " ++ address ++ ":" ++ service

  loopForever () $ \_ -> do
    (initialMsg, sockaddr) <- SB.recvFrom serverSocket tftpMaxPacketSize
    void $ forkIO $ do
      client <- createConnectedUdpSocket sockaddr
      let clientSession = do
            logMsg "creating session"
            handleClient content initialMsg
            logMsg "closing session"
      runReaderT clientSession client
      S.close (clientSocket client)
