{-# LANGUAGE FlexibleInstances #-}

module TftpServer (serveTftp) where

import           Control.Concurrent        (forkIO)
import           Control.Monad             (void)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader      (ReaderT, ask, runReaderT)
import qualified Data.ByteString           as B
import           Data.Functor              ((<&>))
import qualified Network.Socket            as S
import qualified Network.Socket.ByteString as SB
import           System.Timeout            (timeout)

import           TftpConnection

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

  readFileContent fp = do
    content <- liftIO $ (B.readFile fp)
    return $ Just content

-- Network helpers

createConnectedUdpSocket :: S.SockAddr -> IO Client
createConnectedUdpSocket sockaddr = do
  putStrLn (show sockaddr)
  sock <- S.socket addressFamily S.Datagram 0
  S.connect sock sockaddr
  return $ Client sockaddr sock
  where addressFamily = case sockaddr of
                          S.SockAddrInet _ _      -> S.AF_INET
                          S.SockAddrInet6 _ _ _ _ -> S.AF_INET6
                          _                       -> S.AF_UNSPEC

-- Main server loop

loopForever :: a -> (a -> IO a) -> IO ()
loopForever s f = void (loopForever_ s)
  where loopForever_ s_ = f s_ >>= (`loopForever` f)

serveTftp :: S.Socket -> IO ()
serveTftp serverSocket = do
  loopForever () $ \_ -> do
    (initialMsg, sockaddr) <- SB.recvFrom serverSocket tftpMaxPacketSize
    void $ forkIO $ do
      client <- createConnectedUdpSocket sockaddr
      let clientSession = do
            logMsg "creating session"
            handleClient initialMsg
            logMsg "closing session"
      runReaderT clientSession client
      S.close (clientSocket client)
