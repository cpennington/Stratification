module Stratify.Json where

import Stratify.Types
import Data.Aeson
import qualified Data.Aeson.Types as T
import Data.Attoparsec
import qualified Data.ByteString.Char8 as BS


parseJsonFromString :: String -> Either String Value
parseJsonFromString s = case parse json bs of
    (Done rest r) -> Right r
    (Fail rest contexts msg) -> Left msg
    where
        bs = BS.pack s

parseDependenciesFromJson :: Value -> Either String (Dependencies String)
parseDependenciesFromJson = T.parseEither parseJSON
