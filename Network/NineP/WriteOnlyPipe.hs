{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.NineP.WriteOnlyPipe where

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
import Network.NineP.WriteOnlyFile

writeOnlyPipe :: RawFilePath -> FSItemId -> FSItem (Context u)
writeOnlyPipe name index =
  FSItem Occupied (writeOnlyPipeDetails name index) (mkAbsolutePath name) index

writeOnlyPipeDetails :: RawFilePath -> FSItemId -> Details (Context u)
writeOnlyPipeDetails name index =
  Details
  { dOpen = writeOnlyFileOpen
  , dWalk = fileWalk
  , dRead = writeOnlyFileRead
  , dStat = (writeOnlyPipeStat index) {stName = fsItemName name}
  , dReadStat = readStat
  , dWriteStat = writeStat
  , dWrite = sampleWriteToOutReadOpenFids
  , dClunk = fdClunk
  , dAttach = fileAttach
  , dCreate = fdCreate
  , dRemove = writeOnlyFileRemove
  , dVersion = 0
  }

--   , dWrite = fileWrite
writeOnlyPipeStat :: FSItemId -> Stat
writeOnlyPipeStat (FSItemId index) -- if it is not a directory, it is a file
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
writeOnlyPipeOpen
  :: Fid
  -> OpenMode
  -> FidState
  -> FSItem (Context u)
  -> (Context u)
  -> IO (Either NineError (Qid, IOUnit), (Context u))
writeOnlyPipeOpen fid mode fidState me c
  | mode == Write = fileOpen fid mode fidState me c
  | otherwise = return (Left (OtherError "Write Only File"), c)

-- when a file is opened OREAD, then it creates a channel
-- when anything writes to that channel, any reads from that file
--    will read the written data.
-- In this situation, we are trying to write to any read open channels
--    of /out
sampleWriteToOutReadOpenFids
  :: Fid
  -> Offset
  -> ByteString
  -> FSItem s
  -> (Context u)
  -> IO (Either NineError Count, (Context u))
sampleWriteToOutReadOpenFids fid offset bs me c = do
  case IxSet.getOne ((cFSItems c) @= AbsolutePath "/out") of
    Nothing -> return (Left (OtherError "Nothing to write to"), c)
    (Just outFSItem)
    -- write to all /out read channels
     -> do
      writeToOpenChannelsOfFSItemAtIndex (fsItemId outFSItem) bs c
      return ((Right . fromIntegral . BS.length) bs, c)

writeToOpenChannelsOfFSItemAtIndex :: FSItemId
                                   -> ByteString
                                   -> Context u
                                   -> IO ()
writeToOpenChannelsOfFSItemAtIndex i bs c =
  (mapM_ (flip writeToMaybeQueue bs) .
   fmap (\(FidId fid) -> HashMap.lookup fid (cFids c) >>= fidQueue) .
   fmap ffFid . IxSet.toList)
    ((cFSItemFids c) @= i)

writeToMaybeQueue :: Maybe (TQueue ByteString) -> ByteString -> IO ()
writeToMaybeQueue (Nothing) _ = return ()
writeToMaybeQueue (Just q) bs = atomically (writeTQueue q bs)

writeToOpenChannelsOf :: RawFilePath -> ByteString -> (Context u) -> IO ()
writeToOpenChannelsOf fp bs c =
  case IxSet.getOne ((cFSItems c) @= AbsolutePath fp) of
    Nothing -> return ()
    Just fsItem
    -- write to all read channels of FSItem at index
     -> do
      writeToOpenChannelsOfFSItemAtIndex (fsItemId fsItem) bs c
