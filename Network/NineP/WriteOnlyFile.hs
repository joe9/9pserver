{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.NineP.WriteOnlyFile where

import           Control.Concurrent.STM.TQueue
import qualified Data.ByteString                  as BS
import qualified Data.HashMap.Strict              as HashMap
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

writeOnlyFile :: RawFilePath -> FSItemsIndex -> FSItem (Context u)
writeOnlyFile name index = FSItem Occupied (writeOnlyFileDetails name index) []

writeOnlyFileDetails :: RawFilePath -> FSItemsIndex -> Details (Context u)
writeOnlyFileDetails name index =
  Details
  { dOpen = writeOnlyFileOpen
  , dWalk = fileWalk
  , dRead = writeOnlyFileRead
  , dStat = (writeOnlyFileStat index) {stName = fsItemName name}
  , dReadStat = readStat
  , dWriteStat = writeStat
  , dWrite = sampleWriteToOutReadOpenFids
  , dClunk = fdClunk
  , dFlush = fdFlush
  , dAttach = fileAttach
  , dCreate = fdCreate
  , dRemove = writeOnlyFileRemove
  , dVersion = 0
  , dAbsoluteName = fsItemAbsoluteName name
  }

--   , dWrite = fileWrite
writeOnlyFileStat :: FSItemsIndex -> Stat
writeOnlyFileStat index -- if it is not a directory, it is a file
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

writeOnlyFileRemove
  :: Fid
  -> FidState
  -> FSItem (Context u)
  -> (Context u)
  -> (Maybe NineError, (Context u))
writeOnlyFileRemove _ _ _ c = (Just (OtherError "Write Only File"), c)

-- when a file is opened OREAD, then it creates a channel
-- when anything writes to that channel, any reads from that file
--    will read the written data.
-- In this situation, we are trying to write to any read open channels
--    of /out
sampleWriteToOutReadOpenFids
  :: Fid
  -> Offset
  -> ByteString
  -> FidState
  -> FSItem s
  -> (Context u)
  -> IO (Either NineError Count, (Context u))
sampleWriteToOutReadOpenFids fid offset bs fidState me c = do
  let fids = cFids c
  case findIndexUsingName "/out" (cFSItems c) of
    Nothing -> return (Left (OtherError "Nothing to write to"), c)
    (Just outFSItemIndex)
    -- write to all /out read channels
     -> do
      writeToOpenChannelsOfFSItemAtIndex outFSItemIndex bs (cFids c)
      return ((Right . fromIntegral . BS.length) bs, c)

writeToOpenChannelsOfFSItemAtIndex :: FSItemsIndex
                                   -> ByteString
                                   -> HashMap.HashMap Fid FidState
                                   -> IO ()
writeToOpenChannelsOfFSItemAtIndex i bs =
  mapM_ (\f -> writeToMaybeQueue (fidQueue f) bs) .
  HashMap.filter (\f -> i == fidFSItemsIndex f && isJust (fidQueue f))

writeToMaybeQueue :: Maybe (TQueue ByteString) -> ByteString -> IO ()
writeToMaybeQueue (Nothing) _ = return ()
writeToMaybeQueue (Just q) bs = atomically (writeTQueue q bs)
