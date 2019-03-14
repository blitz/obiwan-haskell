{-# LANGUAGE OverloadedStrings #-}

module TftpServer (serveTftp) where

import           Control.Concurrent        (forkIO)
import           Control.Monad             (void)
import qualified Data.ByteString           as B
import           Data.Text                 (pack, unpack)
import qualified Network.Socket            as S
import qualified Network.Socket.ByteString as SB
import           System.Timeout            (timeout)

import           TftpContent               (Content, getContent)
import           TftpProto

-- Configuration

tftpTimeoutMs :: Int
tftpTimeoutMs = 10 * 1000000

tftpMaxPacketSize :: Int
tftpMaxPacketSize = 65536       -- Largest possible IP packet

tftpBlockSize :: Int
tftpBlockSize = 512

-- TFTP State Handling
data Connection
  = Pristine
  | Reading B.ByteString
  deriving (Eq, Show)

type Client = S.SockAddr

-- Get the nth data block for a connection. Block numbers start at 1.
getDataBlock :: Int -> B.ByteString -> Request
getDataBlock n = DTA (fromIntegral n) . getData
  where getData = B.take tftpBlockSize . B.drop ((n - 1) * tftpBlockSize)

continueConnection :: Content -> Connection -> Request -> Maybe (Connection, Request)

-- The client requests to open a file for reading
continueConnection content _ (RRQ filename Binary) = case getContent content (unpack filename) of
  Just buf -> continueConnection content (Reading buf) (ACK 0)
  Nothing  -> Just (Pristine, ERR FileNotFound "No such file")

-- The client acknowledged a data packet. Send the next.
continueConnection _ con@(Reading buf) (ACK n) = Just (con, getDataBlock (fromIntegral n + 1) buf)

-- No clue what happened. Close the connection.
continueConnection _ _ _ = Nothing

-- High-level flow for the TFTP UDP server

putMsg :: Client -> String -> IO ()
putMsg client msg = putStrLn $ show client ++ " | " ++ msg

handleClient :: Content -> Client -> S.Socket -> B.ByteString -> IO ()
handleClient content client socket = handleReceivedData Pristine
  where
    handleReceivedData state msg = maybe closeWithDecodingError (handleDecoded state) $ decodePacket msg
    handleDecoded state decoded =
      maybe closeWithReponseError (uncurry sendReponse) $ continueConnection content state decoded
    sendReponse state resp = do
      case resp of
        (ERR _ errMsg) -> putMsg client $ "error: " ++ unpack errMsg
        _              -> return ()
      SB.sendAll socket $ encodePacket resp
      nextMsg <- timeout tftpTimeoutMs (SB.recvFrom socket tftpMaxPacketSize)
      maybe closeWithTimeout (\(msg, _) -> handleReceivedData state msg) nextMsg

    closeWithDecodingError = closeWithError IllegalOperation "Failed to parse packet"
    closeWithReponseError = closeWithError IllegalOperation "Unknown request"
    closeWithError errCode errMsg = do
      putMsg client ("error: " ++ errMsg)
      SB.sendAll socket $ encodePacket $ ERR errCode (pack errMsg)
    closeWithTimeout = putMsg client "timeout"

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
      handleClient content client clientSocket initialMsg
      putStrLn $ show client ++ " | closing session"
      S.close clientSocket
