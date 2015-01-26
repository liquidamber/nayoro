{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module
  Nayoro.ConfigSpec
  ( spec
  ) where

import Text.Heredoc
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Data.Yaml.Aeson (decodeEither)
import Nayoro.Config

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = do
  describe "Config" $ do
    describe "AppConfig" $ do
        it "should be correctly parsed from minimum config" $
           decodeEither
           [str|sources:
               |  type: "irc"
               |  host: "example.com"
               |] `shouldBe`
           Right (AppConfig { sources=[IRCSourceConfig{host="example.com", port=6667, tls=False, encoding="us-ascii"}], httpConfig = HTTPConfig { httpPort=80} })
        it "should be correctly parsed from maximum config" $
           decodeEither
           [str|sources:
               |  - type: irc
               |    host: foo.example.com
               |  - type: irc
               |    host: bar.example.com
               |    port: 12345
               |    tls: true
               |    encoding: UTF-8
               |http:
               |  port: 3000
               |] `shouldBe`
           Right
           ( AppConfig
             { sources=
               [ IRCSourceConfig { host="foo.example.com", port=6667, tls=False, encoding="us-ascii" }
               , IRCSourceConfig { host="bar.example.com", port=12345, tls=True, encoding="UTF-8" }
               ],
               httpConfig = HTTPConfig { httpPort=3000} })
    describe "IdentitySourceConfig" $ do
      describe "IRCSource" $ do
        it "should be correctly parsed from minimum config" $
          decodeEither
          [str|type: "irc"
              |host: "example.com"
              |] `shouldBe` Right (IRCSourceConfig {host="example.com", port=6667, tls=False, encoding="us-ascii"})
        it "should be correctly parsed from maximum config" $
          decodeEither
            [str|type: "irc"
                |host: "example.com"
                |port: 12345
                |tls: true
                |encoding: UTF-8
                |] `shouldBe`
            Right (IRCSourceConfig {host="example.com", port=12345, tls=True, encoding="UTF-8"})
      describe "AppConfig" $ do
        it "should be correctly parsed w/o sources" $
          decodeEither
          [str|sources:
              |] `shouldSatisfy` (isLeft :: Either a AppConfig -> Bool)
        it "should be correctly parsed w/ 2 sources" $
          decodeEither
          [str|sources:
              | - type: "irc"
              |   host: "foo.example.com"
              | - type: "irc"
              |   host: "bar.example.com"
              |] `shouldBe`
          Right
          ( AppConfig
            { sources =
              [ IRCSourceConfig {host="foo.example.com", port=6667, tls=False, encoding="us-ascii"}
              , IRCSourceConfig {host="bar.example.com", port=6667, tls=False, encoding="us-ascii"}
              ], httpConfig = HTTPConfig
              { httpPort = 80 }})
