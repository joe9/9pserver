{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- Listening on sockets for the incoming requests.
module Network.NineP.Server
  ( run9PServer
  ) where

import           Control.Concurrent.STM.TQueue
import           Control.Exception.Safe        hiding (handle)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BSC
import           Data.IxSet.Typed              as IxSet
import           Data.Serialize                hiding (flush)
import           Data.String.Conversions       (cs)
import qualified GHC.Base                      as Base
import qualified GHC.Show                      as Show
import           Network.Simple.TCP
import           Network.Socket                (socketToHandle)
import           Protolude                     hiding (bracket, get,
                                                handle, msg)
import           System.IO                     (BufferMode (BlockBuffering, LineBuffering, NoBuffering),
                                                Handle,
                                                IOMode (ReadWriteMode),
                                                hClose, hFlush,
                                                hSetBuffering)

-- import Data.String.Conversions
import           Data.NineP
import           Data.NineP.MessageTypes (TransmitMessageType)
import qualified Data.NineP.MessageTypes as MT

-- import Network.NineP.Error
import Network.NineP.Context
import Network.NineP.Response

-- |Run the actual server using the supplied configuration.
-- TODO move the below socket stuff to the Context
run9PServer :: (Context u) -> HostPreference -> ServiceName -> IO ()
run9PServer context hostPreference serviceName
                                   --   hSetBuffering stdout NoBuffering
                                   --   hSetBuffering stderr NoBuffering
 = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  serve hostPreference serviceName $ \(connectionSocket, remoteAddr) -> do
    putText ("TCP connection established from " <> show remoteAddr)
    clientConnection connectionSocket (traceShowId remoteAddr) context

-- Now you may use connectionSocket as you please within this scope,
-- possibly using recv and send to interact with the remote end.
-- got this from
-- https://github.com/glguy/irc-core/blob/v2/src/Client/Network/Async.hs#L152
clientConnection :: Socket -> a -> (Context u) -> IO ()
clientConnection connectionSocket _ context =
  bracket
    (socketToHandle connectionSocket ReadWriteMode)
    hClose
    (\handle -> do
       hSetBuffering handle (BlockBuffering (Just (cMaxMessageSize context)))
       sendQ <- newTQueueIO
       withAsync
         (sendLoop handle sendQ)
         (\sender -> do
            withAsync
              (eventLoop handle sendQ context)
              (\receiver -> do
                 result <- waitEitherCatch sender receiver
                 case result of
                   Left Right {} -> panic "PANIC: sendLoop returned"
                   Right Right {} -> return ()
                   Left (Left e) -> throw e
                   Right (Left e) -> throw e)))

-- TODO Should I even send this when I do not have a tag?
toErrorMessage :: ByteString -> ByteString -> ByteString
toErrorMessage s bs = toNinePFormat (Rerror (BS.concat [s, ": ", bs])) 0

processMessage
  :: MT.TransmitMessageType
  -> Tag
  -> ByteString
  -> (Context u)
  -> (ByteString, (Context u))
processMessage MT.Tversion = process version
processMessage MT.Tattach  = process attach
processMessage MT.Tcreate  = process create
processMessage MT.Tstat    = process stat
processMessage MT.Twstat   = process wstat
processMessage MT.Twalk    = process walk
processMessage _           = undefined

-- TODO Not bothering with max string size.
process
  :: forall t a u.
     (ToNinePFormat a, Serialize t, Show t, Show a)
  => (t -> (Context u) -> (Either Rerror a, (Context u)))
  -> Tag
  -> ByteString
  -> (Context u)
  -> (ByteString, (Context u))
process f tag msg c =
  case runGet get msg of
    Left e -> (toNinePFormat (traceShowId (Rerror (cs e))) tag, c)
    Right d ->
      let result = f (traceShowId d) c
      in case result of
           (Left e, cn)  -> (toNinePFormat (traceShowId e) tag, cn)
           (Right m, cn) -> (toNinePFormat (traceShowId m) tag, cn)

processBlockedReads :: (Context u) -> IO (ByteString, (Context u))
processBlockedReads c = do
  (tagDones, nc) <- checkBlockedReads c
  return ((BS.concat . fmap (uncurry toNinePFormat . swap)) tagDones, nc)

processRead :: TQueue ByteString
            -> Tag
            -> ByteString
            -> (Context u)
            -> IO (Context u)
processRead writeq tag msg c =
  case runGet get msg of
    Left e ->
      (atomically .
       writeTQueue writeq . flip toNinePFormat tag . traceShowId . Rerror . cs)
        e >>
      return c
    Right d -> do
      (readResponse, nc) <- read (traceShowId d) c
      case traceShowId readResponse of
        ReadError e ->
          atomically (writeTQueue writeq (toNinePFormat (Rerror e) tag)) >>
          return nc
        ReadResponse m ->
          atomically (writeTQueue writeq (toNinePFormat (Rread m) tag)) >>
          return nc
        ReadQ readq count fidid -> do
          asyncValue <- async (readFromQ readq count tag writeq)
          return
            (nc
             { cBlockedReads =
                 IxSet.updateIx
                   (BlockedReadTag tag)
                   (BlockedRead (BlockedReadTag tag) count fidid asyncValue)
                   (cBlockedReads nc)
             })

readFromQ :: TQueue ByteString -> Count -> Tag -> TQueue ByteString -> IO Tag
readFromQ readq count tag writeq = do
  atomically
    (do contents <- readTQueue readq
        let (sendContents, forNextTime) =
              BS.splitAt (fromIntegral count) contents
        when (not (BS.null forNextTime)) (unGetTQueue readq forNextTime)
        writeTQueue
          writeq
          (toNinePFormat (traceShowId (Rread sendContents)) tag)
        return tag)

-- TODO Not bothering with max string size.
processIO
  :: forall t a u.
     (ToNinePFormat a, Serialize t, Show a, Show t)
  => (t -> (Context u) -> IO (Either Rerror a, (Context u)))
  -> Tag
  -> ByteString
  -> (Context u)
  -> IO (ByteString, (Context u))
processIO f tag msg c =
  case runGet get msg of
    Left e -> return (toNinePFormat (traceShowId (Rerror (cs e))) tag, c)
    Right d -> do
      eitherResult <- f (traceShowId d) c
      case eitherResult of
        (Left e, cn)  -> return (toNinePFormat e tag, cn)
        (Right m, cn) -> return (toNinePFormat m tag, cn)

--       case traceShow (fst eitherResult) eitherResult of
-- validate if size < maximum size (Nothing about it in intro(5))??
getMessageHeaders :: Get (TransmitMessageType, Tag)
getMessageHeaders = do
  msgType <- getWord8
  tag <- getWord16le
  return (MT.MkTransmitMessageType msgType, tag)

-- TODO could use a proper parser here
-- could do as this
-- https://github.com/glguy/irc-core/blob/v2/src/Client/EventLoop.hs#L73-L79
-- (asum ( [fmap Input readTQueue q ] ++ mapM ( fmap Wait . wait . bAsync ) (cBlockedReads context)))
-- if the input is made to a TQueue too
-- do not feel the need for the input to be a TQueue now.
-- eventLoop :: Handle -> TQueue ByteString -> TQueue ByteString -> (Context u) -> IO () -- (Context u)
-- eventLoop handle input output context = do
eventLoop :: Handle -> TQueue ByteString -> (Context u) -> IO () -- (Context u)
eventLoop handle sendQ context = do
  rawSize <- BS.hGet handle 4
  case runGet getWord32le rawSize of
    Left e ->
      atomically
        (writeTQueue sendQ (toErrorMessage (traceShowId (cs e)) rawSize)) >>
      furtherProcessing handle sendQ context
    Right wsize -> do
      let size = fromIntegral wsize
      if size < 5 -- Do I need this? minimum data required: 4 for size and 1 for tag
        then furtherProcessing handle sendQ context
        else do
          message <- BS.hGet handle (size - 4)
          case runGetState getMessageHeaders message 0 of
            Left e ->
              atomically
                (writeTQueue sendQ (toErrorMessage (traceShowId (cs e)) message)) >>
              furtherProcessing handle sendQ context
            Right ((MT.Tread, tag), msgData) -> do
              updatedContext <- processRead sendQ tag msgData context
              furtherProcessing handle sendQ updatedContext
            Right ((MT.Twrite, tag), msgData) -> do
              (response, updatedContext) <- processIO write tag msgData context
              atomically (writeTQueue sendQ response) >>
                furtherProcessing handle sendQ updatedContext
            Right ((MT.Topen, tag), msgData) -> do
              (response, updatedContext) <- processIO open tag msgData context
              atomically (writeTQueue sendQ response) >>
                furtherProcessing handle sendQ updatedContext
            Right ((MT.Tclunk, tag), msgData) -> do
              (response, updatedContext) <- processIO clunk tag msgData context
              atomically (writeTQueue sendQ response) >>
                furtherProcessing handle sendQ updatedContext
            Right ((MT.Tremove, tag), msgData) -> do
              (response, updatedContext) <- processIO remove tag msgData context
              atomically (writeTQueue sendQ response) >>
                furtherProcessing handle sendQ updatedContext
            Right ((MT.Tflush, tag), msgData) -> do
              (response, updatedContext) <- processIO flush tag msgData context
              atomically (writeTQueue sendQ response) >>
                furtherProcessing handle sendQ updatedContext
            Right ((msgType, tag), msgData) ->
              let (response, updatedContext) =
                    processMessage msgType tag msgData context
                  -- for some reason, the "cs response" is not
                  --    showing the correct value
                  --  putText ("msgType: "
                  --     ++ cs (MT.showTransmitMessageType msgType)
                  --     ++ ", Tag: " ++ show tag
                  --     ++ ", response: " ++ cs response) >>
              in atomically (writeTQueue sendQ response) >>
                 furtherProcessing handle sendQ updatedContext

furtherProcessing :: Handle -> TQueue ByteString -> (Context u) -> IO ()
furtherProcessing handle sendQ c = do
  (errorMessages, nc) <- processBlockedReads c
  atomically (writeTQueue sendQ errorMessages)
  flushAll
  eventLoop handle sendQ nc

sendLoop :: Handle -> TQueue ByteString -> IO ()
sendLoop handle q = do
  bs <- atomically (readTQueue q)
  _ <- BS.hPutNonBlocking handle bs
  hFlush handle
  flushAll
  sendLoop handle q

-- do not see a need for this yet
-- receiveLoop :: Handle -> TQueue ByteString -> IO ()
-- receiveLoop handle q = do
--   bs <- BS.hGet handle 8192
--   atomically (writeTQueue q bs)
--   receiveLoop handle q
flushAll :: IO ()
flushAll = hFlush stdout >> hFlush stderr
