module Main
  ( main
  ) where

import           Data.Semigroup      ((<>))
import           Options.Applicative

import           TftpServer

data Arguments = Arguments
  { tftpAddress :: String
  , tftpService :: String
  }

arguments :: Parser Arguments
arguments =
  Arguments <$>
  strOption
    (long "address" <> metavar "ADDRESS" <> value "127.0.0.1" <> showDefault <>
     help "The address to listen on.") <*>
  strOption
    (long "port" <> metavar "PORT" <> value "tftp" <> showDefault <>
     help "The UDP port the server will listen on")

main :: IO ()
main = do
  config <- execParser opts
  serveTftp (tftpAddress config) (tftpService config)
  where
    opts =
      info
        (arguments <**> helper)
        (fullDesc <> progDesc "Configurable TFTP server")
