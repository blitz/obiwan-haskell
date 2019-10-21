{-# LANGUAGE LambdaCase #-}

module Main (main) where

import           Data.Semigroup ((<>))
import           Network.Socket.Activation as NSA
import           Options.Applicative
import qualified Network.Socket as S

import           TftpServer

-- Command-line parser

data Arguments
  = Standalone String String
  | SocketActivation

standaloneArgs :: Parser Arguments
standaloneArgs = Standalone
  <$> strOption (long "address" <> metavar "ADDRESS" <> value "127.0.0.1" <> showDefault <>
                 help "The address to listen on.")
  <*> strOption (long "port" <> short 'p' <> metavar "PORT" <> value "12345" <> showDefault <>
                 help "The UDP port the server will listen on")

socketActivationArgs :: Parser Arguments
socketActivationArgs = flag' SocketActivation
  (  long "socket-activation"
  <> help "Use systemd socket-activation mode")

arguments :: Parser Arguments
arguments = standaloneArgs <|> socketActivationArgs

createBoundUdpSocket :: String -> String -> IO S.Socket
createBoundUdpSocket address service = do
  putStrLn $ "Listening on " ++ address ++ ":" ++ service
  sock <- S.socket S.AF_INET S.Datagram 0
  addrInfo:_ <-
    S.getAddrInfo
      (Just (S.defaultHints {S.addrSocketType = S.Datagram}))
      (Just address)
      (Just service)
  S.bind sock (S.addrAddress addrInfo)
  return sock

argsToSocket :: Arguments -> IO (S.Socket)
argsToSocket (Standalone address service) = createBoundUdpSocket address service
argsToSocket (SocketActivation) =
  NSA.getActivatedSockets >>= \case
  Just [fd] -> return fd
  otherwise -> fail "Received invalid number of file descriptors"

main :: IO ()
main = do
  putStrLn "Obiwan TFTP Server ready."
  S.withSocketsDo $
    execParser opts >>= argsToSocket >>= serveTftp
  where
    opts = info (arguments <**> helper) (fullDesc <> progDesc "Obiwan Scriptable TFTP Server")
