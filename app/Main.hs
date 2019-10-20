{-# LANGUAGE LambdaCase #-}

module Main (main) where

import qualified Data.ByteString as B
import           Data.ByteString.UTF8 as BSU
import           Data.Semigroup ((<>))
import           Options.Applicative

import           TftpContent (fromKeyValuePairs)
import           TftpServer

data Arguments = Arguments
  { tftpAddress :: String
  , tftpService :: String
  }

arguments :: Parser Arguments
arguments =
  Arguments
  <$> strOption (long "address" <> metavar "ADDRESS" <> value "127.0.0.1" <> showDefault <>
                 help "The address to listen on.")
  <*> strOption (long "port" <> short 'p' <> metavar "PORT" <> value "12345" <> showDefault <>
                 help "The UDP port the server will listen on")

main :: IO ()
main = do
  config <- execParser opts

  let kvContent = [("foo", BSU.fromString "bar")]

  serveTftp (tftpAddress config) (tftpService config) (fromKeyValuePairs kvContent)
  where
    opts = info (arguments <**> helper) (fullDesc <> progDesc "Obiwan Scriptable TFTP Server")
