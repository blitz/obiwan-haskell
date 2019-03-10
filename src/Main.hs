{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad             (void)
import qualified Data.ByteString           as B
import           Data.Functor              ((<&>))
import           Data.List                 ((\\))
import qualified Data.Map.Strict           as Map
import qualified Data.PQueue.Min           as PQ
import qualified Data.Set                  as Set
import           Debug.Trace               (trace)
import qualified Network.Socket            as S
import qualified Network.Socket.ByteString as SB
import           System.Clock

import           TftpProto

-- Configuration

tftpServerPort :: String
tftpServerPort = "12345"

tftpTestData :: B.ByteString
tftpTestData = "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?"

tftpBlockSize :: Int
tftpBlockSize = 512

-- TFTP State Handling

data Connection = Pristine
                | Reading B.ByteString
  deriving (Eq, Show)

type Client = S.SockAddr
type TftpState = Map.Map Client Connection

emptyTftpState :: TftpState
emptyTftpState = Map.empty

-- Get the nth data block for a connection. Block numbers start at 1.
getDataBlock :: Int -> B.ByteString -> Request
getDataBlock n = DTA (fromIntegral n) . getData
  where getData = B.take tftpBlockSize . B.drop ((n - 1) * tftpBlockSize)

continueConnection :: Request -> Connection -> Maybe (Connection, Request)
continueConnection (ACK n) con@(Reading buf) = Just (con, getDataBlock (fromIntegral n + 1) buf)
continueConnection (RRQ _ Binary) _ = continueConnection (ACK 0) (Reading tftpTestData)
continueConnection _ _ = Nothing

initiateConnection :: Client -> Request -> Maybe (Connection, Request)
initiateConnection c req = trace (show c ++ ": " ++ show req) (continueConnection req Pristine)

handleMessage :: TftpState -> Client -> Request -> (TftpState, Maybe Request)
handleMessage s client req = maybe new continue (Map.lookup client s)
  where
    continue con = maybe (Map.delete client s, Nothing) store (continueConnection req con)
    new = maybe failed store (initiateConnection client req)
    failed = errorOut s IllegalOperation "Establishing connection failed"
    store (connection, resp) = (Map.insert client connection s, Just resp)
    errorOut ns errCode msg =
      trace (show client ++ ": " ++ show msg) (ns, Just (ERR errCode msg))

removeClients :: TftpState -> [Client] -> TftpState
removeClients s = Map.withoutKeys s . Set.fromList

-- Timeout Handling
-- TODO Retransmissions

-- We use the boottime clock, because it is monotonic and also counts sleep times.
timeoutClock :: Clock
timeoutClock = Boottime

-- After this much of silence from a client, we forget connection state.
connectionTimeout :: TimeSpec
connectionTimeout = TimeSpec 10 0

-- Remember the time of the last packet of a client
type TimeoutQueue = PQ.MinQueue (TimeSpec, Client)

emptyTimeoutState :: TimeoutQueue
emptyTimeoutState = PQ.empty

-- Update the timeout for a specific client.
updateTimeout :: TimeoutQueue -> Client -> TimeSpec -> TimeoutQueue
updateTimeout s client now = PQ.insert (now, client) $ PQ.filter isNotReplaced s
  where isNotReplaced (_, c) = c /= client

-- Find all clients that timed out, remove them from the timeout list and return
-- them.
processTimeouts :: TimeoutQueue -> TimeSpec -> ([Client], TimeoutQueue)
processTimeouts s now = (map snd tuples, newQueue)
  where (tuples, newQueue) = PQ.span isOld s
        isOld (time, _) = time < timeoutAbs
        timeoutAbs = now - connectionTimeout

-- Control Flow

maybeDo :: Maybe a -> (a -> IO ()) -> IO ()
maybeDo m f = maybe (return ()) f m

loopForever :: a -> (a -> IO a) -> IO ()
loopForever s f = void (loopForever_ s)
  where loopForever_ s_ = f s_ >>= (`loopForever` f)

handlePacket :: S.Socket -> Client -> B.ByteString -> TftpState -> IO TftpState
handlePacket sock client msg state =
  case decodePacket msg <&> handleMessage state client of
      Just (newTftpState, maybeReq) -> do
        maybeDo maybeReq (\buf -> SB.sendAllTo sock (encodePacket buf) client)
        return newTftpState
      Nothing ->
        return state

handleTimeouts :: TimeoutQueue -> Client -> TimeSpec -> ([Client], TimeoutQueue)
handleTimeouts s client now = processTimeouts (updateTimeout s client now) now

main :: IO ()
main = S.withSocketsDo $ do
  sock <- S.socket S.AF_INET S.Datagram 0
  addrInfo:_ <- S.getAddrInfo (Just (S.defaultHints { S.addrSocketType = S.Datagram}))
              (Just "127.0.0.1") (Just tftpServerPort)
  S.bind sock (S.addrAddress addrInfo)

  loopForever (emptyTftpState, emptyTimeoutState) $ \(state, timeouts) -> do
    (msg, client) <- SB.recvFrom sock 1500

    (expiredClients, newTimeoutState) <- handleTimeouts timeouts client <$> getTime timeoutClock
    newState <- handlePacket sock client msg $ removeClients state expiredClients

    return (newState, newTimeoutState)
