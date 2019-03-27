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

type Client = S.SockAddr

instance MonadIO m => MonadTftpConnection (ReaderT S.Socket m) where
  recvData timeoutMs = do
    socket <- ask
    liftIO $ timeout timeoutMs $ SB.recvFrom socket tftpMaxPacketSize <&> fst

  sendData buf = do
    socket <- ask
    liftIO $ SB.sendAll socket buf

  logMsg msg = liftIO $ putStrLn msg

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

createConnectedUdpSocket :: Client -> IO S.Socket
createConnectedUdpSocket client = do
  sock <- S.socket S.AF_INET S.Datagram 0
  S.connect sock client
  return sock

-- Main server loop

loopForever :: a -> (a -> IO a) -> IO ()
loopForever s f = void (loopForever_ s)
  where loopForever_ s_ = f s_ >>= (`loopForever` f)

serveTftp :: String -> String -> Content -> IO ()
serveTftp address service content = S.withSocketsDo $ do
  serverSocket <- createBoundUdpSocket address service
  putStrLn $ "Listening on " ++ address ++ ":" ++ service

  loopForever () $ \_ -> do
    (initialMsg, client) <- SB.recvFrom serverSocket tftpMaxPacketSize
    void $ forkIO $ do
      putStrLn $ show client ++ " | creating session"
      clientSocket <- createConnectedUdpSocket client
      runReaderT (handleClient content initialMsg) clientSocket
      putStrLn $ show client ++ " | closing session"
      S.close clientSocket
