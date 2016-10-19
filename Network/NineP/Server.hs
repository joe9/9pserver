{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- Listening on sockets for the incoming requests.
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.NineP.Server
  ( run9PServer
  ) where

import           Control.Exception.Safe  hiding (handle)
import           Data.ByteString         (ByteString)
import           Protolude hiding (bracket, get, handle, msg)
import qualified Data.ByteString         as BS
import           Data.Serialize hiding (flush)
import           Data.String.Conversions
import           Network.Simple.TCP
import           Network.Socket          (socketToHandle)
import           System.IO               (BufferMode (NoBuffering),
                                          Handle,
                                          IOMode (ReadWriteMode),
                                          hClose, hSetBuffering)

-- import Data.String.Conversions
import           Data.NineP
import           Data.NineP.MessageTypes (TransmitMessageType)
import qualified Data.NineP.MessageTypes as MT

-- import Network.NineP.Error
import Network.NineP.Context
import Network.NineP.Response

-- |Run the actual server using the supplied configuration.
-- TODO move the below socket stuff to the Context
run9PServer :: Context -> IO ()
run9PServer context = do
  serve (Host "127.0.0.1") "5960" $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "TCP connection established from " ++ show remoteAddr
    clientConnection connectionSocket remoteAddr context
    -- Now you may use connectionSocket as you please within this scope,
    -- possibly using recv and send to interact with the remote end.

clientConnection :: Socket -> t -> Context -> IO ()
clientConnection connectionSocket _ context =
    bracket
      (socketToHandle connectionSocket ReadWriteMode)
      hClose
      (\handle -> hSetBuffering handle NoBuffering >> receiver handle context)

-- TODO Should I even send this when I do not have a tag?
sendErrorMessage :: Handle -> ByteString -> ByteString -> IO ()
sendErrorMessage h s bs =
  BS.hPut h (toNinePFormat (Rerror (BS.concat [cs s, ": ", bs])) 0)

processMessage :: MT.TransmitMessageType
               -> Tag
               -> ByteString
               -> Context
               -> (ByteString, Context)
processMessage MT.Tversion = process version
processMessage MT.Tattach = process attach
processMessage MT.Tclunk = process clunk
processMessage MT.Tflush = process flush
processMessage MT.Tremove = process remove
processMessage MT.Tcreate = process create
processMessage MT.Tstat = process rstat
processMessage MT.Twstat = process wstat
processMessage MT.Twalk = process walk
processMessage _ = undefined

-- TODO Not bothering with max string size.
process
  :: forall t a.
     (ToNinePFormat a, Serialize t)
  => (t -> Context -> (Either Rerror a, Context))
  -> Tag
  -> ByteString
  -> Context
  -> (ByteString, Context)
process f tag msg c =
  case runGet get msg of
    Left e -> (toNinePFormat (Rerror (cs e)) tag, c)
    Right d ->
      case f d c of
        (Left e, cn)  -> (toNinePFormat e tag, cn)
        (Right m, cn) -> (toNinePFormat m tag, cn)

-- TODO Not bothering with max string size.
processIO
  :: forall t a.
     (ToNinePFormat a, Serialize t)
  => (t -> Context -> IO (Either Rerror a, Context))
  -> Tag
  -> ByteString
  -> Context
  -> IO (ByteString, Context)
processIO f tag msg c =
  case runGet get msg of
    Left e -> return (toNinePFormat (Rerror (cs e)) tag, c)
    Right d -> do
      eitherResult <- f d c
      case eitherResult of
        (Left e, cn)  -> return (toNinePFormat e tag, cn)
        (Right m, cn) -> return (toNinePFormat m tag, cn)

-- validate if size < maximum size (Nothing about it in intro(5))??
getMessageHeaders :: Get (TransmitMessageType, Tag)
getMessageHeaders = do
  msgType <- getWord8
  tag <- getWord16le
  return (MT.MkTransmitMessageType msgType, tag)

receiver :: Handle -> Context -> IO () -- Context
receiver handle context = do
  rawSize <- BS.hGet handle 4
  case runGet getWord32le rawSize of
    Left e -> sendErrorMessage handle (cs e) rawSize >> receiver handle context
    Right wsize ->
      let size = fromIntegral wsize
      in if size < 5 -- Do I need this? minimum data required: 4 for size and 1 for tag
           then receiver handle context
           else do
             message <- BS.hGet handle (size - 4)
             case runGetState getMessageHeaders message (size - 4) of
               Left e ->
                 sendErrorMessage handle (cs e) message >> receiver handle context
               Right ((MT.Tread, tag), msgData) -> do
                 (response, updatedContext) <-
                       processIO read tag msgData context
                 BS.hPut handle response >> receiver handle updatedContext
               Right ((MT.Twrite, tag), msgData) -> do
                 (response, updatedContext) <-
                       processIO write tag msgData context
                 BS.hPut handle response >> receiver handle updatedContext
               Right ((MT.Topen, tag), msgData) -> do
                 (response, updatedContext) <-
                       processIO open tag msgData context
                 BS.hPut handle response >> receiver handle updatedContext
               Right ((msgType, tag), msgData) ->
                 let (response, updatedContext) =
                       processMessage msgType tag msgData context
                 in BS.hPut handle response >> receiver handle updatedContext
