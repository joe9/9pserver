{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.NineP.Functions where

import           Control.Concurrent.STM.TQueue
import qualified Data.ByteString                  as BS
import           Data.Default
import qualified Data.HashMap.Strict              as HashMap
import           Data.List
import           Data.Serialize
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as V
import qualified Data.Vector.Mutable              as DVM
import           Protolude                        hiding (put)
import           System.Posix.ByteString.FilePath
import           System.Posix.FilePath

-- import           Text.Groom
-- import           GHC.Show
import BitMask

import           Data.NineP
import           Data.NineP.OpenMode
import qualified Data.NineP.OpenMode as OpenMode
import           Data.NineP.Qid      hiding (Directory)
import qualified Data.NineP.Qid      as Qid
import           Data.NineP.Stat     hiding (Directory)
import qualified Data.NineP.Stat     as Stat

import Network.NineP.Context
import Network.NineP.Error

sampleContext
  :: Default u
  => (Context u)
sampleContext = def {cFSItems = sampleFSItemsList}

sampleFSItemsList :: V.Vector (FSItem (Context u))
sampleFSItemsList =
  V.fromList
    [ sampleDir "/" 0
    , sampleFile "/in" 1
    , sampleFile "/out" 2
    , sampleDir "/dir1" 3
    , sampleFile "/dir1/in" 4
    ]

sampleDir, sampleFile :: RawFilePath -> FSItemsIndex -> FSItem (Context u)
sampleDir name index = FSItem Occupied (dirDetails name index) []

sampleFile name index = FSItem Occupied (sampleFileDetails name index) []

resetContext :: (Context u) -> (Context u)
resetContext c = c {cFids = HashMap.empty}

-- TODO need some validation to ensure that the parent directory exists
-- name is an absolute path
-- use readOnlyFileDetails or writeOnlyFileDetails instead of this
sampleFileDetails, dirDetails :: RawFilePath
                              -> FSItemsIndex
                              -> Details (Context u)
sampleFileDetails name index =
  Details
  { dOpen = fileOpen
  , dWalk = fileWalk
  , dRead = fileRead
  , dStat = (fileStat index) {stName = fsItemName name}
  , dReadStat = readStat
  , dWriteStat = writeStat
  , dWrite = undefined -- fileWrite
  , dClunk = fdClunk
  , dFlush = fdFlush
  , dAttach = fileAttach
  , dCreate = fdCreate
  , dRemove = fileRemove
  , dVersion = 0
  , dAbsoluteName = fsItemAbsoluteName name
  }

dirDetails name index =
  Details
  { dOpen = dirOpen
  , dWalk = dirWalk
  , dRead = dirRead
  , dStat = (dirStat index) {stName = fsItemName name}
  , dReadStat = readStat
  , dWriteStat = writeStat
  , dWrite = dirWrite
  , dClunk = fdClunk
  , dFlush = fdFlush
  , dAttach = dirAttach
  , dCreate = fdCreate
  , dRemove = dirRemove
  , dVersion = 0
  , dAbsoluteName = fsItemAbsoluteName name
  }

dirRemove
  :: Fid
  -> FidState
  -> FSItem (Context u)
  -> (Context u)
  -> (Maybe NineError, (Context u))
dirRemove _ _ _ c = (Just (OtherError "Not implemented"), c)

dirWrite
  :: Fid
  -> Offset
  -> ByteString
  -> FidState
  -> FSItem (Context u)
  -> (Context u)
  -> IO (Either NineError Count, (Context u))
dirWrite _ _ _ _ _ c = return (Left (OtherError "Not implemented"), c)

fdFlush :: FSItem (Context u) -> (Context u) -> (Context u)
fdFlush _ c = c

fsItemName :: RawFilePath -> RawFilePath
fsItemName name
  | isRelative name =
    panic "fsItemName: file or directory name must be absolute"
  | name == "/" = name
  | hasTrailingPathSeparator name =
    takeFileName (dropTrailingPathSeparator name)
  | otherwise = takeFileName name

fsItemAbsoluteName :: RawFilePath -> RawFilePath
fsItemAbsoluteName name
  | isRelative name =
    panic "fsItemName: file or directory name must be absolute"
  | name == "/" = name
  | hasTrailingPathSeparator name = dropTrailingPathSeparator name
  | otherwise = name

-- NineError message
-- TODO change all the undefineds to return the old context and a
noneDetails :: Details (Context u)
noneDetails =
  Details
  { dOpen = undefined
  , dWalk = undefined
  , dRead = undefined
  , dStat = undefined
  , dReadStat = undefined
  , dWriteStat = undefined
  , dWrite = undefined
  , dClunk = undefined
  , dFlush = undefined
  , dAttach = undefined
  , dCreate = undefined
  , dRemove = undefined
  , dVersion = 0
  , dAbsoluteName = ""
  }

none :: FSItem (Context u)
none = FSItem Vacant noneDetails []

-- fileOpen :: Fid -> OpenMode -> s -> (Either NineError Qid, s)
-- fileOpen _ _ context = (Left (ENotImplemented "fileOpen"), context)
-- fileWalk :: NineError
-- fileWalk = ENotADir
-- fileRead :: Fid -> Offset -> Length -> s -> (Either NineError B.ByteString, s)
-- fileRead _ _ _ context = (Left (ENotImplemented "fileOpen"), context)
-- fileWrite :: Fid -> Offset -> ByteString -> FidState -> FSItem s -> s -> IO (Either NineError Count, s)
-- fileWrite fid offset bs (FidState _ i c)
-- fileWrite _ _ _ context = (Left (ENotImplemented "fileOpen"), context)
fdClunk :: Fid
        -> FSItem (Context u)
        -> (Context u)
        -> (Maybe NineError, (Context u))
fdClunk fid _ c = (Nothing, c {cFids = HashMap.delete fid (cFids c)})

-- fileFlush :: s -> s
-- fileFlush context = context
dirAttach
  :: Fid
  -> AFid
  -> UserName
  -> AccessName
  -> FSItemsIndex
  -> FSItem (Context u)
  -> (Context u)
  -> (Either NineError Qid, (Context u))
dirAttach fid _ _ _ i d c =
  ( Right ((stQid . dStat . fDetails) d)
  , c {cFids = HashMap.insert fid (FidState Nothing Nothing i) (cFids c)})

fileAttach
  :: Fid
  -> AFid
  -> UserName
  -> AccessName
  -> FSItemsIndex
  -> FSItem (Context u)
  -> (Context u)
  -> (Either NineError Qid, (Context u))
fileAttach _ _ _ _ _ _ c =
  (Left (OtherError "fileAttach: file cannot be attached"), c)

fdCreate
  :: Fid
  -> ByteString
  -> Permissions
  -> OpenMode
  -> FSItem (Context u)
  -> (Context u)
  -> (Either NineError (Qid, IOUnit), (Context u))
fdCreate _ _ _ _ _ context = (Left (ENotImplemented "fileCreate"), context)

-- TODO check for permissions, etc
-- TODO bug creating fid should happen in walk, not here
-- TODO when opened for reading, create and add the TQueue here
fileOpen
  :: Fid
  -> OpenMode
  -> FidState
  -> FSItem (Context u)
  -> (Context u)
  -> IO (Either NineError (Qid, IOUnit), (Context u))
fileOpen fid mode fidState me c
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
  | otherwise = return (Right ((stQid . dStat . fDetails) me, iounit), c)
  where
    iounit = fromIntegral ((cMaxMessageSize c) - 23) -- maximum size of each message

-- TODO check for permissions, etc
-- TODO bug creating fid should happen in walk, not here
-- TODO when opened for reading, create and add the TQueue here
dirOpen
  :: Fid
  -> OpenMode
  -> FidState
  -> FSItem (Context u)
  -> (Context u)
  -> IO (Either NineError (Qid, IOUnit), (Context u))
dirOpen fid _ fidState me c =
  let iounit = fromIntegral ((cMaxMessageSize c) - 23) -- maximum size of each message
  in return
       ( Right ((stQid . dStat . fDetails) me, iounit)
       , c
         {cFids = HashMap.insert fid (fidState {fidQueue = Nothing}) (cFids c)})

-- fileWrite
--   :: Fid
--   -> Offset
--   -> ByteString
--   -> FidState
--   -> FSItem s
--   -> s
--   -> IO (Either NineError Count, s)
-- fileWrite _ _ _ (FidState Nothing _) _ c =
--   return ((Left . OtherError) "No Queue to write to", c)
-- fileWrite _ _ bs (FidState (Just q) _) _ c = do
--   atomically (writeTQueue q bs)
--   return ((Right . fromIntegral . BS.length) bs, c)
-- TODO check for permissions, iounit details, etc
fileRead
  :: Fid
  -> Offset
  -> Count
  -> FidState
  -> FSItem (Context u)
  -> (Context u)
  -> IO (ReadResponse, (Context u))
fileRead _ _ _ (FidState Nothing _ _) _ c =
  return ((ReadError . showNineError . OtherError) "No Queue to read from", c)
fileRead _ _ count (FidState (Just q) _ _) _ c = return (ReadQ q count, c)

-- TODO check for permissions, iounit details, etc
dirRead
  :: Fid
  -> Offset
  -> Count
  -> FidState
  -> FSItem (Context u)
  -> (Context u)
  -> IO (ReadResponse, (Context u))
dirRead fid 0 count fidState fsItem c =
  let fsItems = cFSItems c
      -- remove the trailing slash of the directory
      dirname = (dAbsoluteName . fDetails) fsItem
      childrenStats =
        (V.map (dStat . fDetails) .
         V.filter (belongsToDir (traceShowId dirname)))
          fsItems
      childrenStatsBS = V.map (runPut . put) (traceShowId childrenStats)
      toSendBS = (BS.concat . V.toList) childrenStatsBS
  in return
       ( ReadResponse (BS.take (fromIntegral count) toSendBS)
       , c
         { cFids =
             HashMap.insert
               fid
               (fidState {fidResponse = Just toSendBS})
               (cFids c)
         })
dirRead _ offset count fidState _ c =
  let responseBS =
        maybe
          BS.empty
          (BS.take (fromIntegral count) . BS.drop (fromIntegral offset))
          (fidResponse fidState)
  in return (ReadResponse responseBS, c)

belongsToDir :: RawFilePath -> FSItem (Context u) -> Bool
belongsToDir fp fsItem
                -- is the same item
  | fp == (dAbsoluteName . fDetails) fsItem = False
  | otherwise = fp == (takeDirectory . dAbsoluteName . fDetails) fsItem

-- TODO http://man2.aiju.de/5/remove -- What is the behaviour if the concerned fid is a directory? remove the directory? how about any files in that directory?  [20:34]
fileRemove
  :: Fid
  -> FidState
  -> FSItem (Context u)
  -> (Context u)
  -> (Maybe NineError, (Context u))
fileRemove fid fidState _ c =
  let index = fidFSItemsIndex fidState
  in ( Nothing
     , c
       { cFids = HashMap.delete fid (cFids c)
       , cFSItems = V.modify (\v -> DVM.write v index none) (cFSItems c)
       })

readStat :: Fid -> FSItem s -> s -> (Either NineError Stat, s)
readStat _ me context = ((Right . dStat . fDetails) me, context)

-- TODO check permissions when doing this
-- TODO , stQid    = Qid -- TODO
writeStat
  :: Fid
  -> Stat
  -> FidState
  -> FSItem (Context u)
  -> (Context u)
  -> (Maybe NineError, (Context u))
writeStat _ stat fidState me c =
  let oldstat = (dStat . fDetails) me
      updatedStat =
        oldstat
        { stTyp =
            if stTyp stat == 0xffff -- don't touch value Word16
              then stTyp oldstat
              else stTyp stat
        , stDev =
            if stDev stat == 0xffffffff -- don't touch value Word32
              then stDev oldstat
              else stDev stat
        , stMode =
            if toBitMask (stMode stat) == 0xffffffff --don't touch value Word32
              then stMode oldstat
              else stMode stat
        , stAtime =
            if stAtime stat == 0xffffffff -- don't touch value Word32
              then stAtime oldstat
              else stAtime stat
        , stMtime =
            if stMtime stat == 0xffffffff -- don't touch value Word32
              then stMtime oldstat
              else stMtime stat
        , stLength =
            if stLength stat == 0xffffffffffffffff -- don't touch value Word64
              then stLength oldstat
              else stLength stat
        , stName -- should not be changing this
           =
            if BS.null (stName stat) -- don't touch value == ""
              then stName oldstat
              else stName stat
        , stUid =
            if BS.null (stUid stat) -- don't touch value == ""
              then stUid oldstat
              else stUid stat
        , stGid =
            if BS.null (stGid stat) -- don't touch value == ""
              then stGid oldstat
              else stGid stat
        , stMuid =
            if BS.null (stMuid stat) -- don't touch value == ""
              then stMuid oldstat
              else stMuid stat
        }
      newFSItem = me {fDetails = (fDetails me) {dStat = updatedStat}}
      index = fidFSItemsIndex fidState
      updatedContext =
        c {cFSItems = V.modify (\v -> DVM.write v index newFSItem) (cFSItems c)}
  in (Nothing, updatedContext)

dirStat :: FSItemsIndex -> Stat
dirStat index =
  Stat
  { stTyp = 0
  , stDev = 0
  , stQid = Qid [Qid.Directory] 0 index
  , stMode =
      [ OtherExecutePermission
      , OtherWritePermission
      , OtherReadPermission
      , GroupExecutePermission
      , GroupWritePermission
      , GroupReadPermission
      , UserExecutePermission
      , UserWritePermission
      , UserReadPermission
      , Stat.Directory
      ]
  , stAtime = 0
  , stMtime = 0
  , stLength = 0
  , stName = "/"
  , stUid = "root"
  , stGid = "root"
  , stMuid = "root"
  }

fileStat :: FSItemsIndex -> Stat
fileStat index -- if it is not a directory, it is a file
 =
  Stat
  { stTyp = 0
  , stDev = 0
  , stQid = Qid [Qid.AppendOnly] 0 index
  , stMode =
      [ OtherExecutePermission
      , OtherWritePermission
      , OtherReadPermission
      , GroupExecutePermission
      , GroupWritePermission
      , GroupReadPermission
      , UserExecutePermission
      , UserWritePermission
      , UserReadPermission
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

-- initialStat _ index = nullStat index
vacantStat :: FSItemsIndex -> Stat
vacantStat index =
  Stat
  { stTyp = 0
  , stDev = 0
  , stQid = Qid [Qid.AppendOnly] 0 index
  , stMode =
      [ OtherExecutePermission
      , OtherWritePermission
      , OtherReadPermission
      , GroupExecutePermission
      , GroupWritePermission
      , GroupReadPermission
      , UserExecutePermission
      , UserWritePermission
      , UserReadPermission
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

-- TODO implement ".." - walk to the parent directory
-- fdWalk
--   :: Fid
--   -> NewFid
--   -> [ByteString]
--   -> FidState
--   -> FSItem Context
--   -> Context
--   -> (Either NineError [Qid], Context)
-- fdWalk _ newfid [] fidState _ c =
--   ( Right []
--   , c {cFids = HashMap.insert newfid (fidState {fidQueue = Nothing}) (cFids c)})
-- fdWalk fid newfid nwnames _ d c
--   | V.length fsItems > 1 =
--     ( Left
--         (OtherError
--            (BS.append
--               "dirWalk: Multiple FSItems with the same name found"
--               (joinPath nwnames)))
--     , c)
--   | V.length fsItems == 0 =
--     let qids =
--           traceShowId
--             ((catMaybes .
--               takeWhile isJust .
--               fmap (buildQid (cFSItems c)) . scanl' combine fidName)
--                nwnames)
--     in (Right qids, c)
--   | otherwise =
--     let qids =
--           traceShowId
--             ((catMaybes . fmap (buildQid (cFSItems c)) . scanl' combine fidName)
--                nwnames)
--     in ( Right qids
--        , c
--          { cFids =
--              HashMap.insert newfid (FidState Nothing fsItemIndex) (cFids c)
--          })
--   where
--     fidName = (stName . dStat . fDetails) d
--     fsItems =
--       traceShowId
--         (V.findIndices
--            (hasName (traceShowId (combine fidName (joinPath nwnames))))
--            (cFSItems c))
--     fsItemIndex = V.head (traceShowId fsItems)
--     fsItem = fromMaybe d ((cFSItems c) V.!? fsItemIndex)
-- convert of "//" to "/"
normalizePath :: RawFilePath -> RawFilePath
normalizePath = joinPath . splitDirectories

-- buildQid :: Vector (FSItem Context) -> RawFilePath -> Maybe Qid
-- buildQid fsItems path =
--   V.findIndex (hasName path) fsItems >>=
--   (\fsItemIndex ->
--      (fsItems V.!? fsItemIndex) >>= \fsitem ->
--        Just
--          (Qid
--             (stModeToQType fsitem)
--             ((dVersion . fDetails) fsitem)
--             (fromIntegral fsItemIndex)))
hasName :: RawFilePath -> FSItem (Context u) -> Bool
hasName name fsitem =
  let normalizedName = normalizePath name
  in normalizedName == (normalizePath . dAbsoluteName . fDetails) fsitem

fileWalk
  :: NewFid
  -> RawFilePath
  -> [Qid] -- parent qids
  -> [RawFilePath] -- still to traverse
  -> (Context u)
  -> (Either NineError [Qid], (Context u))
fileWalk newfid name parentQids [] c =
  case findIndexUsingName name (cFSItems c) of
    Nothing -> (Right parentQids, c)
    Just fsItemsIndex ->
      ( Right parentQids
      , c
        { cFids =
            HashMap.insert
              newfid
              (FidState Nothing Nothing fsItemsIndex)
              (cFids c)
        })
fileWalk _ _ parentQids _ c = (Right parentQids, c)

findIndexUsingName :: RawFilePath -> Vector (FSItem (Context u)) -> Maybe Int
findIndexUsingName name = V.findIndex (hasName name)

dirWalk
  :: NewFid
  -> RawFilePath
  -> [Qid] -- parent qids
  -> [RawFilePath] -- still to traverse
  -> (Context u)
  -> (Either NineError [Qid], (Context u))
dirWalk newfid name parentQids [] c =
  case findIndexUsingName name (cFSItems c) of
    Nothing -> (Right parentQids, c)
    Just fsItemsIndex ->
      ( Right parentQids
      , c
        { cFids =
            HashMap.insert
              newfid
              (FidState Nothing Nothing fsItemsIndex)
              (cFids c)
        })
dirWalk newfid name parentQids (f:fs) c =
  case findIndexUsingName (combine name f) (cFSItems c) of
    Nothing -> (Right parentQids, c)
    Just fsItemsIndex ->
      case (cFSItems c) V.!? fsItemsIndex of
        Nothing -> (Right parentQids, c)
        Just fsItem ->
          ((dWalk . fDetails) fsItem)
            newfid
            (combine name f)
            (parentQids ++ [(stQid . dStat . fDetails) fsItem])
            fs
            c
