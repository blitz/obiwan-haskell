module Main (main) where

import           Data.Semigroup      ((<>))
import           Options.Applicative

import           TftpServer

newtype Arguments = Arguments {
  tftpService :: String
  }

arguments :: Parser Arguments
arguments = Arguments
  <$> strOption (long "port" <> metavar "PORT" <> value "tftp" <> showDefault
                    <> help "The UDP port the server will listen on")

main :: IO ()
main = do
  config <- execParser opts
  serveTftp (tftpService config)
  where opts = info (arguments <**> helper) (fullDesc <> progDesc "Configurable TFTP server")
