{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad             (void)
import           Data.ByteString           as B
import           Data.Functor              ((<&>))
import           Data.Map.Strict           as Map
import qualified Network.Socket            as S
import qualified Network.Socket.ByteString as SB
import           TftpProto

-- Configuration

tftpServerPort :: String
tftpServerPort = "12345"

tftpTestData :: B.ByteString
tftpTestData = "This is very long test data"

-- State Handling

getReply :: Request -> Maybe Request
getReply (RRQ _ Ascii) = Just (ERR IllegalOperation "ASCII mode not supported")
getReply (RRQ _ Binary) = Just (ERR FileNotFound "No such file")
getReply (ACK _)          = Nothing
getReply _                = Nothing

data Connection = Reading B.ByteString

type Client = S.SockAddr
type State = Map.Map Client Connection

emptyState :: State
emptyState = Map.empty

handleMessage :: State -> Client -> Request -> (State, Maybe Request)
handleMessage s c r = (s, getReply r)

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
