{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.NineP.ReadOnlyFile where

import           Protolude                        hiding (put)
import           System.Posix.ByteString.FilePath

import           Data.NineP
import           Data.NineP.Qid
import           Data.NineP.Stat
import           Data.NineP.OpenMode

import Network.NineP.Context
import Network.NineP.Error
import Network.NineP.Functions

readOnlyFile :: RawFilePath -> FSItemsIndex -> FSItem (Context u)
readOnlyFile name index = FSItem Occupied (readOnlyFileDetails name index) []

readOnlyFileDetails :: RawFilePath
                                -> FSItemsIndex
                                -> Details (Context u)
readOnlyFileDetails name index =
  Details
  { dOpen = readOnlyFileOpen
  , dWalk = fileWalk
  , dRead = fileRead
  , dStat = (readOnlyFileStat index) {stName = fsItemName name}
  , dReadStat = readStat
  , dWriteStat = writeStat
  , dWrite = readOnlyFileWrite
  , dClunk = fdClunk
  , dFlush = fdFlush
  , dAttach = fileAttach
  , dCreate = fdCreate
  , dRemove = readOnlyFileRemove
  , dVersion = 0
  , dAbsoluteName = fsItemAbsoluteName name
  }

readOnlyFileStat :: FSItemsIndex -> Stat
readOnlyFileStat index -- if it is not a directory, it is a file
 =
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
readOnlyFileOpen
  :: Fid
  -> OpenMode
  -> FidState
  -> FSItem (Context u)
  -> (Context u)
  -> IO (Either NineError (Qid, IOUnit), (Context u))
readOnlyFileOpen fid mode fidState me c
  | mode == Read = fileOpen fid mode fidState me c
  | otherwise = return (Left (OtherError "Read Only File"), c)

readOnlyFileWrite
  :: Fid
  -> Offset
  -> ByteString
  -> FidState
  -> FSItem s
  -> s
  -> IO (Either NineError Count, s)
readOnlyFileWrite _ _ _ _ _ c = return (Left (OtherError "Read Only File"), c)

readOnlyFileRemove :: Fid
                   -> FidState
                   -> FSItem (Context u)
                   -> (Context u)
                   -> (Maybe NineError, (Context u))
readOnlyFileRemove _ _ _ c = (Just (OtherError "Read Only File"), c)
