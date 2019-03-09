{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad             (void)
import           Data.ByteString           as B
import           Data.Functor              ((<&>))
import           Data.Map.Strict           as Map
import           Debug.Trace               (trace)
import qualified Network.Socket            as S
import qualified Network.Socket.ByteString as SB
import           TftpProto

-- Configuration

tftpServerPort :: String
tftpServerPort = "12345"

tftpTestData :: B.ByteString
tftpTestData = "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?"

tftpBlockSize :: Int
tftpBlockSize = 512

-- State Handling

newtype Connection = Reading B.ByteString

type Client = S.SockAddr
type State = Map.Map Client Connection

emptyState :: State
emptyState = Map.empty

-- Get the nth data block for a connection. Block numbers start at 1.
getDataBlock :: Int -> B.ByteString -> Request
getDataBlock n = DTA (fromIntegral n) . getData
  where getData = B.take tftpBlockSize . B.drop ((n - 1) * tftpBlockSize)

continueConnection :: Request -> Connection -> Maybe (Connection, Request)
continueConnection (ACK n) con@(Reading buf) = Just (con, getDataBlock (fromIntegral n + 1) buf)
continueConnection _ _ = Nothing

initiateConnection :: Client -> Request -> Maybe (Connection, Request)
initiateConnection _ (RRQ _ Binary) = continueConnection (ACK 0) (Reading tftpTestData)
initiateConnection _ _ = Nothing

initiateConnectionWithLog :: Client -> Request -> Maybe (Connection, Request)
initiateConnectionWithLog c req = trace ("New connection from " ++ show c ++ " " ++ show req) (initiateConnection c req)

handleMessage :: State -> Client -> Request -> (State, Maybe Request)
handleMessage s client req = maybe new continue (Map.lookup client s)
  where
    continue con = maybe (delete client s, Nothing) store (continueConnection req con)
    new = maybe failed store (initiateConnectionWithLog client req)
    failed = errorOut s IllegalOperation "Establishing connection failed"
    store (connection, resp) = (Map.insert client connection s, Just resp)
    errorOut ns errCode msg = (ns, Just (ERR errCode msg))

-- Control Flow

maybeDo :: Maybe a -> (a -> IO ()) -> IO ()
maybeDo m f = maybe (return ()) f m

loopForever :: State -> (State -> IO State) -> IO ()
loopForever s f = void (loopForever_ s)
  where loopForever_ s_ = f s_ >>= (`loopForever` f)

main :: IO ()
main = S.withSocketsDo $ do
  sock <- S.socket S.AF_INET S.Datagram 0
  addrInfo:_ <- S.getAddrInfo (Just (S.defaultHints { S.addrSocketType = S.Datagram}))
              (Just "127.0.0.1") (Just tftpServerPort)
  S.bind sock (S.addrAddress addrInfo)

  loopForever emptyState $ \state -> do
    (msg, client) <- SB.recvFrom sock 1500
    case decodePacket msg <&> handleMessage state client of
      Just (newState, maybeReq) -> do
        maybeDo maybeReq (\buf -> SB.sendAllTo sock (encodePacket buf) client)
        return newState
      Nothing ->
        return state
