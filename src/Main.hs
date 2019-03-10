module Main
  ( main
  ) where

import           Data.Semigroup      ((<>))
import           Options.Applicative

import           TftpServer

data Arguments = Arguments
  { tftpAddress :: String
  , tftpService :: String
  , luaScript   :: FilePath
  }

arguments :: Parser Arguments
arguments =
  Arguments
  <$> strOption (long "address" <> metavar "ADDRESS" <> value "127.0.0.1" <> showDefault <>
                 help "The address to listen on.")
  <*> strOption (long "port" <> short 'p' <> metavar "PORT" <> value "tftp" <> showDefault <>
                 help "The UDP port the server will listen on")
  <*> strOption (long "script" <> short 's' <> metavar "FILE" <>
                 help "A Lua script that populates the content the TFTP server will serve")

main :: IO ()
main = do
  config <- execParser opts
  serveTftp (tftpAddress config) (tftpService config)
  where
    opts =
      info
        (arguments <**> helper)
        (fullDesc <> progDesc "Obiwan Scriptable TFTP Server")
