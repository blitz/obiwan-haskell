{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  ) where

import           Data.Semigroup      ((<>))
import qualified Foreign.Lua         as Lua
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
  <*> strOption (long "port" <> short 'p' <> metavar "PORT" <> value "12345" <> showDefault <>
                 help "The UDP port the server will listen on")
  <*> strOption (long "script" <> short 's' <> metavar "FILE" <> value "examples/helloworld.lua" <> showDefault <>
                 help "A Lua script that populates the content the TFTP server will serve")

main :: IO ()
main = do
  config <- execParser opts

  Lua.run $ do
    Lua.openlibs
    Lua.loadfile (luaScript config) >>=
      \case
        Lua.OK -> Lua.call 0 0
        e -> error ("Failed to load Lua script: " ++ show e)

  serveTftp (tftpAddress config) (tftpService config)
  where
    opts =
      info
        (arguments <**> helper)
        (fullDesc <> progDesc "Obiwan Scriptable TFTP Server")
