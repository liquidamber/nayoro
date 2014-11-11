{-# LANGUAGE OverloadedStrings #-}

module
  Nayoro.ConfigSpec
  ( spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Data.Yaml.Aeson (decodeEither)
import Nayoro.Config

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = do
  describe "Config" $ do
    describe "IdentitySourceConfig" $ do
      describe "IRCSource" $ do
        it "should be correctly parsed from minimum config" $
          decodeEither
          "type: \"irc\"\nhost: \"example.com\"\n"
          `shouldBe` Right (IRCSourceConfig {host="example.com", port=6667, tls=False})
        it "should be correctly parsed from maximum config" $
          decodeEither
            "type: \"irc\"\nhost: \"example.com\"\nport: 12345\ntls: true\n"
            `shouldBe`
            Right (IRCSourceConfig {host="example.com", port=12345, tls=True})
      describe "AppConfig" $ do
        it "should be correctly parsed w/o sources" $
          decodeEither
          "sources:\n"
          `shouldSatisfy` (isLeft :: Either a AppConfig -> Bool)
        it "should be correctly parsed w/ 2 sources" $
          decodeEither
          "sources:\n - type: \"irc\"\n   host: \"foo.example.com\"\n - type: \"irc\"\n   host: \"bar.example.com\"\n"
          `shouldBe`
          Right (AppConfig {sources = [IRCSourceConfig {host = "foo.example.com", port = 6667, tls = False}, IRCSourceConfig {host = "bar.example.com", port = 6667, tls = False}] })
