{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module TftpConnection
  ( handleClient
  , MonadTftpConnection(..)
  ) where

import           Control.Monad.State.Lazy (State, runState, state)
import qualified Data.ByteString          as B
import           Data.Functor             ((<&>))
import           Data.Maybe               (catMaybes)

import           TftpContent              (Content, getContent)
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

handleClient :: MonadTftpConnection m => Content -> B.ByteString -> m ()
handleClient content = handleReceivedData Pristine
  where
    handleReceivedData conn msg = maybe closeWithDecodingError (handleDecoded conn) $ decodePacket msg
    handleDecoded conn decoded =
      maybe closeWithReponseError (uncurry sendReponse) $ continueConnection content conn decoded
    sendReponse resp conn = do
      case resp of
        (ERR _ errMsg) -> logMsg $ "error: " ++ errMsg
        _              -> return ()
      sendData $ encodePacket resp
      nextMsg <- recvData tftpTimeoutMs
      maybe closeWithTimeout (handleReceivedData conn) nextMsg

    closeWithDecodingError = closeWithError IllegalOperation "Failed to parse packet"
    closeWithReponseError = closeWithError IllegalOperation "Unknown request"
    closeWithError errCode errMsg = do
      logMsg  $ "error: " ++ errMsg
      sendData $ encodePacket $ ERR errCode errMsg
    closeWithTimeout = logMsg "timeout"
