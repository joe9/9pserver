{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.NineP.WriteOnlyFile where

import           Control.Concurrent.STM.TQueue
import qualified Data.ByteString                  as BS
import qualified Data.HashMap.Strict              as HashMap
import           Data.IxSet.Typed
import qualified Data.IxSet.Typed                 as IxSet
import           Protolude                        hiding (put)
import           System.Posix.ByteString.FilePath

import           Data.NineP
import           Data.NineP.OpenMode
import           Data.NineP.Qid
import qualified Data.NineP.Qid      as Qid
import           Data.NineP.Stat
import qualified Data.NineP.Stat     as Stat

import Network.NineP.Context
import Network.NineP.Error
import Network.NineP.Functions

writeOnlyFile :: RawFilePath -> FSItemId -> FSItem (Context u)
writeOnlyFile name index =
  FSItem Occupied (writeOnlyFileDetails name index) (mkAbsolutePath name) index

writeOnlyFileDetails :: RawFilePath -> FSItemId -> Details (Context u)
writeOnlyFileDetails name index =
  Details
  { dOpen = writeOnlyFileOpen
  , dWalk = fileWalk
  , dRead = writeOnlyFileRead
  , dStat = (writeOnlyFileStat index) {stName = fsItemName name}
  , dReadStat = readStat
  , dWriteStat = writeStat
  , dWrite = writeOnlyFileWrite
  , dClunk = fdClunk
  , dFlush = fdFlush
  , dAttach = fileAttach
  , dCreate = fdCreate
  , dRemove = writeOnlyFileRemove
  , dVersion = 0
  }

--   , dWrite = fileWrite
writeOnlyFileStat :: FSItemId -> Stat
writeOnlyFileStat (FSItemId index) -- if it is not a directory, it is a file
 =
  Stat
  { stTyp = 0
  , stDev = 0
  , stQid = Qid [Qid.AppendOnly] 0 index
  , stMode =
      [ OtherWritePermission
      , GroupWritePermission
      , UserWritePermission
      , Stat.AppendOnly
      ]
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
writeOnlyFileOpen
  :: Fid
  -> OpenMode
  -> FidState
  -> FSItem (Context u)
  -> (Context u)
  -> IO (Either NineError (Qid, IOUnit), (Context u))
writeOnlyFileOpen fid mode fidState me c
  | mode == Write = fileOpen fid mode fidState me c
  | otherwise = return (Left (OtherError "Write Only File"), c)

writeOnlyFileRead :: Fid
                  -> Offset
                  -> Count
                  -> FidState
                  -> FSItem s
                  -> s
                  -> IO (ReadResponse, s)
writeOnlyFileRead _ _ _ _ _ c =
  return ((ReadError . showNineError . OtherError) "Write Only File", c)

writeOnlyFileRemove :: Fid
                    -> FSItem (Context u)
                    -> (Context u)
                    -> IO (Maybe NineError, (Context u))
writeOnlyFileRemove _ _ c = return (Just (OtherError "Write Only File"), c)

writeOnlyFileWrite
  :: Fid
  -> Offset
  -> ByteString
  -> FSItem s
  -> (Context u)
  -> IO (Either NineError Count, (Context u))
writeOnlyFileWrite _ _ _ _ c =
  return (Left (ENotImplemented "writeOnlyFileWrite: "), c)
