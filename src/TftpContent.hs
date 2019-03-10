module TftpContent (
    Content
  , FileRule(..)
  , fromKeyValuePairs
  , getContent
  ) where

import qualified Data.ByteString    as B
import           Data.Maybe         (listToMaybe, mapMaybe)
import           System.FilePattern (FilePattern, match)

data FileRule =
  File FilePattern
       B.ByteString

type Content = [FileRule]

-- TODO This needs to cover more use cases, including variable substitution. Or
-- should this be handled in Lua?
fromKeyValuePairs :: [(String, B.ByteString)] -> Content
fromKeyValuePairs = map toRule
  where
    toRule (ptrn, content) = File ptrn content

getContent :: Content -> FilePath -> Maybe B.ByteString
getContent rules path = listToMaybe matchingRules
  where matchingRules = mapMaybe matchRule rules
        matchRule (File ptrn content) = match ptrn path >> return content

