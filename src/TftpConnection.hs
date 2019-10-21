{-# LANGUAGE FlexibleInstances #-}

module TftpConnection
  ( handleClient
  , MonadTftpConnection(..)
  ) where

import           Control.Monad.State.Lazy (State, runState, state)
import qualified Data.ByteString as B
import           Data.Functor ((<&>))
import           Data.Maybe (catMaybes)

import           TftpProto

-- Configuration

type TimeoutMs = Int

tftpTimeoutMs :: TimeoutMs
tftpTimeoutMs = 10 * 1000000

type Blocksize = Int

tftpBlockSize :: Blocksize
tftpBlockSize = 512

-- Interface to the outside world

class Monad m => MonadTftpConnection m where
  recvData :: TimeoutMs -> m (Maybe B.ByteString)
  sendData :: B.ByteString -> m ()
  logMsg :: String -> m ()
  -- TODO This needs to be a lazy ByteString.
  readFileContent :: FilePath -> m (Maybe B.ByteString)

-- TFTP State Handling

data Connection
  = Pristine
  | Reading B.ByteString Blocksize
  deriving (Eq, Show)

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

-- Any options we acknowledge need to be sent back to the client in an OACK
-- packet. We can modify them as well, i.e. by reducing the block size in a
-- blksize option.
acknowledgeOptions :: Connection -> [RequestOption] -> (Request, Connection)
acknowledgeOptions conn options = runState (mapM ackOption options <&> catMaybes <&> OACK) conn
  where
    ackOption :: RequestOption -> State Connection (Maybe RequestOption)
    ackOption o = state (acknowledgeOption o)

continueConnection :: MonadTftpConnection m => Connection -> Request -> m (Request, Connection)

-- The client requests to open a file for reading
continueConnection _ (RRQ filename Binary options) = do
  fileContent <- readFileContent filename
  case fileContent of
    Just buf -> if null options then
                  continueConnection newState (ACK 0)
                else
                  return $ acknowledgeOptions newState options
      where newState = Reading buf tftpBlockSize
    Nothing  -> return $ (ERR FileNotFound "No such file", Pristine)

-- The client acknowledged a data packet. Send the next.
continueConnection con@(Reading buf blksize) (ACK n) = return (getDataBlock (fromIntegral n + 1) blksize buf, con)

-- Invalid request
continueConnection con _ = do
  logMsg (show con)
  return $ (ERR IllegalOperation "Unknown request", Pristine)

handleOne :: MonadTftpConnection m => Connection -> B.ByteString -> m Connection
handleOne state msg = do
  case decodePacket msg of
    Nothing -> closeWithDecodingError
    Just packet -> do
      logMsg (show packet)
      (response, newState) <- continueConnection state packet
      sendReponse response
      return $ newState
  
  where
    sendReponse resp = do
      case resp of
        (ERR _ errMsg) -> logMsg $ "error: " ++ errMsg
        _              -> return ()
      sendData $ encodePacket resp

    closeWithDecodingError = closeWithError IllegalOperation "Failed to parse packet"
    closeWithError errCode errMsg = do
      logMsg  $ "error: " ++ errMsg
      sendData $ encodePacket $ ERR errCode errMsg
      return Pristine

messageLoop :: MonadTftpConnection m => Connection -> B.ByteString -> m ()
messageLoop state msg = do
  newState <- handleOne state msg
  maybeNewMsg <- recvData tftpTimeoutMs

  case maybeNewMsg of
    Just newMsg -> messageLoop newState newMsg
    Nothing -> do
      logMsg "timeout"
      return ()

handleClient :: MonadTftpConnection m => B.ByteString -> m ()
handleClient = messageLoop Pristine
