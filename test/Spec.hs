{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Simple.TCP
import Protolude
import Test.Tasty         (defaultMain, testGroup)

import Network.NineP

import qualified Response.Tests
import           Server.Tests

-- import qualified Keymap.CustomDvorak.Tests
main :: IO ()
main = do
  let context = testContext
  withAsync
    (run9PServer context (Host "127.0.0.1") "5961")
    (\_ -> do
       threadDelay 1000000
       connect
         "127.0.0.1"
         "5961"
         (\(connectionSocket, remoteAddr) -> do
            putText ("Connection established to " <> show remoteAddr)
            defaultMain
              (testGroup
                 "Tests"
                 [Response.Tests.tests, Server.Tests.tests connectionSocket])))
