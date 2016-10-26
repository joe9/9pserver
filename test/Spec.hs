{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Exception.Safe
import Network.Simple.TCP
import Protolude
-- import System.IO.Silently
import System.IO.Capture
import Test.Tasty             (defaultMain, testGroup)

import Network.NineP

import qualified Response.Tests
import           Server.Tests

-- import qualified Keymap.CustomDvorak.Tests
-- https://github.com/hspec/silently
main :: IO ()
main = do
  withAsync
     silent9PServer
     (\_ -> do
        threadDelay 1000
        connect
            "127.0.0.1"
            "5961"
            (\(connectionSocket, remoteAddr) -> do
                putStrLn ( "Connection established to " ++ show remoteAddr)
                defaultMain
                    (testGroup
                        "Tests"
                        [Response.Tests.tests, Server.Tests.tests connectionSocket])))

silent9PServer :: IO ()
silent9PServer = do
  let context = testContext
  (captured, _,_,_) <-
    capture
        (catchAny -- catch the "cancel" sent by the tests thread
            (run9PServer context (Host "127.0.0.1") "5961")
                (\e -> do putStrLn ("Got an exception: " ++ show e)))
  putStr "captured: " >> putStrLn captured
