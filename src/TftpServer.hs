{-# LANGUAGE OverloadedStrings #-}

module TftpServer (serveTftp) where

import           Control.Concurrent        (forkIO)
import           Control.Monad             (void)
import           Control.Monad.State.Lazy  (State, runState, state)
import qualified Data.ByteString           as B
import           Data.Functor              ((<&>))
import           Data.Maybe                (catMaybes)
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

type Blocksize = Int

tftpBlockSize :: Blocksize
tftpBlockSize = 512

-- TFTP State Handling
data Connection
  = Pristine
  | Reading B.ByteString Blocksize
  deriving (Eq, Show)

type Client = S.SockAddr

-- Get the nth data block for a connection. Block numbers start at 1.
getDataBlock :: Int -> Blocksize -> B.ByteString -> Request
getDataBlock n blksize = DTA (fromIntegral n) . getData
  where getData = B.take blksize . B.drop ((n - 1) * blksize)

updateBlksize :: Connection -> Blocksize -> Connection
updateBlksize (Reading dta _) blksize = Reading dta blksize
updateBlksize Pristine _              = Pristine

acknowledgeOption :: RequestOption -> Connection -> (Maybe RequestOption, Connection)
acknowledgeOption blkOpt@(BlksizeOption blksize) conn = (Just blkOpt, updateBlksize conn blksize)
acknowledgeOption _ conn = (Nothing, conn)

acknowledgeOptions :: Connection -> [RequestOption] -> (Request, Connection)
acknowledgeOptions conn options = runState ((mapM ackOption options <&> catMaybes) <&> OACK) conn
  where
    ackOption :: RequestOption -> State Connection (Maybe RequestOption)
    ackOption o = state (acknowledgeOption o)

continueConnection :: Content -> Connection -> Request -> Maybe (Request, Connection)

-- The client requests to open a file for reading
continueConnection content _ (RRQ filename Binary options) =
  case getContent content filename of
    Just buf -> if null options then
                  continueConnection content newState (ACK 0)
                else
                  Just (acknowledgeOptions newState options)
      where newState = Reading buf tftpBlockSize
    Nothing  -> Just (ERR FileNotFound "No such file", Pristine)

-- The client acknowledged a data packet. Send the next.
continueConnection _ con@(Reading buf blksize) (ACK n) = Just (getDataBlock (fromIntegral n + 1) blksize buf, con)

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
    sendReponse resp state = do
      case resp of
        (ERR _ errMsg) -> putMsg client $ "error: " ++ errMsg
        _              -> return ()
      SB.sendAll socket $ encodePacket resp
      nextMsg <- timeout tftpTimeoutMs (SB.recvFrom socket tftpMaxPacketSize)
      maybe closeWithTimeout (\(msg, _) -> handleReceivedData state msg) nextMsg

    closeWithDecodingError = closeWithError IllegalOperation "Failed to parse packet"
    closeWithReponseError = closeWithError IllegalOperation "Unknown request"
    closeWithError errCode errMsg = do
      putMsg client ("error: " ++ errMsg)
      SB.sendAll socket $ encodePacket $ ERR errCode errMsg
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
