-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- Listening on sockets for the incoming requests.
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Network.NineP.Server
    ( module Network.NineP.Internal.File
    , Config(..)
    , run9PServer
    ) where

import Control.Exception.Safe
import Data.String.Conversions
import Data.Serialize
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word
import Data.String.Conversions
import Network.Simple.TCP
import Network.Socket               (socketToHandle)
import System.IO                    (BufferMode (NoBuffering),
                                     IOMode (ReadWriteMode), hClose,
                                     hGetLine, hPutStrLn,
                                     hSetBuffering, Handle)

import Data.NineP
import qualified Data.NineP.MessageTypes as MT
import Data.NineP.MessageTypes (TransmitMessageType)

import Network.NineP.Error
import Network.NineP.Internal.Context
-- import Network.NineP.Internal.File
-- import Network.NineP.Internal.Msg
-- import Network.NineP.Internal.State

-- |Run the actual server using the supplied configuration.
run9PServer :: Context -> IO ()
run9PServer context = do
  serve (Host "127.0.0.1") "5960" $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "TCP connection established from " ++ show remoteAddr
    -- Now you may use connectionSocket as you please within this scope,
    -- possibly using recv and send to interact with the remote end.
    bracket (socketToHandle connectionSocket ReadWriteMode)
      hClose
      (\handle -> hSetBuffering handle NoBuffering >> receiver handle context)

-- TODO Should I even send this when I do not have a tag?
sendErrorMessage :: Handle -> String -> ByteString -> IO ()
sendErrorMessage h s bs = BS.hPut h (toNinePFormat (Rerror (BS.concat [cs s, ": ", bs])) 0)

processMessage :: MT.TransmitMessageType -> Tag -> ByteString -> Context -> ( ByteString, Context)
processMessage MT.Tversion tag msg = process rversion tag msg
processMessage _ _ _ = undefined

-- process :: a -> Tag ->
process f tag msg =
  case runGet get msg of
        Left e -> (toNinePFormat (rerror e),c)
        Right d -> case f d of
                    ( Left e,c) -> (toNinePFormat ( rerror e),c)
                    ( Right m,c) ->  (toNinePFormat m,c)

-- validate if size < maximum size (Nothing about it in intro(5))??
getMessageHeaders :: Get (TransmitMessageType, Tag)
getMessageHeaders = do
        msgType <- getWord8
        tag <- getWord16le
        return (MT.toTransmitMessageType msgType, tag)

receiver :: Handle -> Context -> IO () -- Context
receiver handle context = do
  rawSize <- BS.hGet handle 4
  case runGet getWord32le rawSize of
    Left e -> sendErrorMessage handle e rawSize >> receiver handle context
    Right wsize ->
      let size = fromIntegral wsize
      in if size < 5 -- Do I need this? minimum data required: 4 for size and 1 for tag
            then receiver handle context
            else do
                message <- BS.hGet handle (size - 4)
                case runGetState getMessageHeaders message (size - 4) of
                    Left e -> sendErrorMessage handle e message >> receiver handle context
                    Right ((msgType, tag), msgData) ->
                        let (response, updatedContext) = processMessage msgType tag msgData context
                        in BS.hPut handle response >> receiver handle updatedContext
