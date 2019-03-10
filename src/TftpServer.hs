{-# LANGUAGE OverloadedStrings #-}

module TftpServer (serveTftp) where

import           Control.Concurrent        (forkIO)
import           Control.Monad             (void)
import qualified Data.ByteString           as B
import           Data.String               (fromString)
import qualified Network.Socket            as S
import qualified Network.Socket.ByteString as SB
import           System.Timeout            (timeout)

import           TftpProto

-- Configuration

tftpTimeoutMs :: Int
tftpTimeoutMs = 10 * 1000000

tftpMaxPacketSize :: Int
tftpMaxPacketSize = 9000

tftpTestData :: B.ByteString
tftpTestData = "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?"

tftpBlockSize :: Int
tftpBlockSize = 512

-- TFTP State Handling

data Connection = Pristine
                | Reading B.ByteString
  deriving (Eq, Show)

type Client = S.SockAddr

-- Get the nth data block for a connection. Block numbers start at 1.
getDataBlock :: Int -> B.ByteString -> Request
getDataBlock n = DTA (fromIntegral n) . getData
  where getData = B.take tftpBlockSize . B.drop ((n - 1) * tftpBlockSize)

continueConnection :: Request -> Connection -> Maybe (Connection, Request)
continueConnection (ACK n) con@(Reading buf) = Just (con, getDataBlock (fromIntegral n + 1) buf)
continueConnection (RRQ _ Binary) _ = continueConnection (ACK 0) (Reading tftpTestData)
continueConnection _ _ = Nothing

-- High-level flow for the TFTP UDP server

handleClient :: Client -> S.Socket -> B.ByteString -> IO ()
handleClient client socket = handleReceivedData Pristine
  where
    handleReceivedData state msg = maybe closeWithDecodingError (handleDecoded state) $ decodePacket msg
    handleDecoded state decoded =
      maybe closeWithReponseError (uncurry sendReponse) $ continueConnection decoded state
    sendReponse state resp = do
      SB.sendAll socket $ encodePacket resp
      nextMsg <- timeout tftpTimeoutMs (SB.recvFrom socket tftpMaxPacketSize)
      maybe closeWithTimeout (\(msg, _) -> handleReceivedData state msg) nextMsg

    closeWithDecodingError = closeWithError IllegalOperation "Failed to parse packet"
    closeWithReponseError = closeWithError IllegalOperation "Unknown request"
    closeWithError errCode errMsg = do
      putStrLn (show client ++ " triggered error " ++ errMsg)
      SB.sendAll socket $ encodePacket $ ERR errCode (fromString errMsg)
    closeWithTimeout = putStrLn (show client ++ " reached timeout.")

createBoundUdpSocket :: String -> String -> IO S.Socket
createBoundUdpSocket address service = do
  sock <- S.socket S.AF_INET S.Datagram 0
  addrInfo:_ <- S.getAddrInfo (Just (S.defaultHints { S.addrSocketType = S.Datagram}))
                (Just address) (Just service)
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

serveTftp :: String -> String -> IO ()
serveTftp address service = S.withSocketsDo $ do
  serverSocket <- createBoundUdpSocket address service

  loopForever () $ \_ -> do
    (initialMsg, client) <- SB.recvFrom serverSocket tftpMaxPacketSize
    void $ forkIO $ do
      putStrLn $ "Creating socket for " ++ show client
      clientSocket <- createConnectedUdpSocket client
      handleClient client clientSocket initialMsg
      putStrLn $ "Closing connection for " ++ show client
      S.close clientSocket
