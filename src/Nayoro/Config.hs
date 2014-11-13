{-# LANGUAGE OverloadedStrings #-}
module
  Nayoro.Config
  ( AppConfig(..)
  , IdentitySourceConfig(..)
  , loadConfigFromFile
  ) where

import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Yaml.Aeson as Yaml
import Data.Yaml.Aeson ((.:), (.:?), (.!=))

-- |
-- Configuration of whole application.
data AppConfig
  = AppConfig
    { sources :: [IdentitySourceConfig] -- ^ Identity sources.
    }
  deriving (Show, Read, Eq)

instance Yaml.FromJSON AppConfig where
  parseJSON (Yaml.Object o) = do
    sources_ <- o .: "sources" >>= \srcs -> case srcs of
      Yaml.Object _ -> Yaml.parseJSON srcs
      -- rewrite as Applicative
      Yaml.Array v -> mapM Yaml.parseJSON (Vector.toList v)
      _ -> fail "sources shuold be object/array"
    return AppConfig { sources = sources_ }
  parseJSON _ = do
    fail "App config should be object"

-- |
-- Configuration of identity sources.
data IdentitySourceConfig
     -- | Config for IRC identity source.
  = IRCSourceConfig
    { host :: Text.Text
    , port :: Int
    , tls :: Bool
    , encoding :: Text.Text
    }
  deriving (Show, Read, Eq)

instance Yaml.FromJSON IdentitySourceConfig where
  parseJSON (Yaml.Object o) = do
    source_type <- o .: "type" .!= ""
    case source_type `asTypeOf` Text.empty of
      "irc" -> parseIRCSource o
      _ -> fail $ "unknown source type: " ++ (Text.unpack source_type)
  parseJSON _ = fail "source element should be object"

parseIRCSource :: Yaml.Object -> Yaml.Parser IdentitySourceConfig
parseIRCSource o = do
  host_ <- o .: "host"
  port_ <- o .:? "port" .!= 6667
  tls_ <- o .:? "tls" .!= False
  encoding_ <- o .:? "encoding" .!= "us-ascii"
  return IRCSourceConfig
    { host = host_
    , port = port_
    , tls = tls_
    , encoding = encoding_
    }

loadConfigFromFile :: FilePath -> IO (Either Yaml.ParseException AppConfig)
loadConfigFromFile = Yaml.decodeFileEither
