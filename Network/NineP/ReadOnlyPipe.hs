{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.NineP.ReadOnlyPipe where

import           Control.Concurrent.STM.TQueue
import qualified Data.HashMap.Strict              as HashMap
import           Protolude                        hiding (put)
import           System.Posix.ByteString.FilePath

import Data.NineP
import Data.NineP.OpenMode
import Data.NineP.Qid
import Data.NineP.Stat

import Network.NineP.Context
import Network.NineP.Error
import Network.NineP.Functions
import Network.NineP.ReadOnlyFile

readOnlyPipe :: RawFilePath -> FSItemId -> FSItem (Context u)
readOnlyPipe name index =
  FSItem Occupied (readOnlyPipeDetails name index) (mkAbsolutePath name) index

readOnlyPipeDetails :: RawFilePath -> FSItemId -> Details (Context u)
readOnlyPipeDetails name index =
  Details
  { dOpen = readOnlyPipeOpen
  , dWalk = fileWalk
  , dRead = readOnlyPipeRead
  , dStat = (readOnlyPipeStat index) {stName = fsItemName name}
  , dReadStat = readStat
  , dWriteStat = writeStat
  , dWrite = readOnlyFileWrite
  , dClunk = fdClunk
  , dFlush = fdFlush
  , dAttach = fileAttach
  , dCreate = fdCreate
  , dRemove = readOnlyFileRemove
  , dVersion = 0
  }

readOnlyPipeRead
  :: Fid
  -> Offset
  -> Count
  -> FidState
  -> FSItem (Context u)
  -> (Context u)
  -> IO (ReadResponse, (Context u))
readOnlyPipeRead _ _ _ (FidState Nothing _ _) _ c =
  return ((ReadError . showNineError . OtherError) "No Queue to read from", c)
readOnlyPipeRead _ _ count (FidState (Just q) _ fidId) _ c =
  return (ReadQ q count fidId, c)

-- if it is not a directory, it is a file
readOnlyPipeStat :: FSItemId -> Stat
readOnlyPipeStat (FSItemId index) =
  Stat
  { stTyp = 0
  , stDev = 0
  , stQid = Qid [] 0 index
  , stMode = [OtherReadPermission, GroupReadPermission, UserReadPermission]
  , stAtime = 0
  , stMtime = 0
  , stLength = 0
  , stName = ""
  , stUid = "root"
  , stGid = "root"
  , stMuid = "root"
  }

-- TODO check for permissions, etc
-- TODO bug creating fid should happen in walk, not here
-- TODO when opened for reading, create and add the TQueue here
readOnlyPipeOpen
  :: Fid
  -> OpenMode
  -> FidState
  -> FSItem (Context u)
  -> (Context u)
  -> IO (Either NineError (Qid, IOUnit), (Context u))
readOnlyPipeOpen fid mode fidState me c
  | mode == Read && isJust (fidQueue fidState) -- OREAD and Q already exists
   = return (Right ((stQid . dStat . fDetails) me, iounit), c)
  | mode == Read -- OREAD
   = do
    readQ <- newTQueueIO
    return
      ( Right ((stQid . dStat . fDetails) me, iounit)
      , c
        { cFids =
            HashMap.insert fid (fidState {fidQueue = Just readQ}) (cFids c)
        })
  | otherwise = return (Left (OtherError "Read Only Pipe"), c)
  where
    iounit = fromIntegral ((cMaxMessageSize c) - 23) -- maximum size of each message
