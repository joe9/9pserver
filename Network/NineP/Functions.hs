{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.NineP.Functions where

import           Control.Concurrent.STM.TQueue
import qualified Data.ByteString                  as BS
import           Data.Default
import           qualified Data.HashMap.Strict              as HashMap
import           Data.List
import           Data.Serialize
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as V
import qualified Data.Vector.Mutable              as DVM
import           GHC.Show
import           Protolude                        hiding (put)
import           System.Posix.ByteString.FilePath
import           System.Posix.FilePath
import           Text.Groom

import           Data.NineP
import           Data.NineP.Qid hiding (Directory, File)
import qualified Data.NineP.Qid as Qid
import           Data.NineP.Stat  hiding (Directory)
import qualified Data.NineP.Stat  as Stat

import Network.NineP.Context
import Network.NineP.Error

sampleContext :: Context
sampleContext = def {cFSItems = sampleFSItemsList}

sampleFSItemsList :: V.Vector (FSItem Context)
sampleFSItemsList =
  V.fromList
    [ sampleDir "/" 0
    , sampleFile "/in" 1
    , sampleFile "/out" 2
    , sampleDir "/dir1" 3
    , sampleFile "/dir1/in" 4
    ]

sampleDir, sampleFile :: RawFilePath -> FSItemsIndex -> FSItem Context
sampleDir name index = FSItem Directory (dirDetails name index) []

sampleFile name index = FSItem File (fileDetails name index) []

resetContext :: Context -> Context
resetContext c = c {cFids = HashMap.empty}

-- TODO need some validation to ensure that the parent directory exists
-- name is an absolute path
fileDetails, dirDetails :: RawFilePath -> FSItemsIndex -> Details Context
fileDetails name index =
  Details
  { dOpen = fileOpen
  , dWalk = fileWalk
  , dRead = fileRead
  , dStat = (fileStat index) {stName = fileName name}
  , dReadStat = readStat
  , dWriteStat = writeStat
  , dWrite = undefined
  , dClunk = fdClunk
  , dFlush = undefined
  , dAttach = fileAttach
  , dCreate = fdCreate
  , dRemove = fileRemove
  , dVersion = 0
  }

dirDetails name index =
  Details
  { dOpen = dirOpen
  , dWalk = dirWalk
  , dRead = dirRead
  , dStat = (dirStat index) {stName = dirName name}
  , dReadStat = readStat
  , dWriteStat = writeStat
  , dWrite = undefined
  , dClunk = fdClunk
  , dFlush = undefined
  , dAttach = dirAttach
  , dCreate = fdCreate
  , dRemove = undefined
  , dVersion = 0
  }

dirName :: RawFilePath -> RawFilePath
dirName name
  | isRelative name = panic "dirName: directory name must be absolute"
  | name == "/" = name
  | hasTrailingPathSeparator name = name
  | otherwise = addTrailingPathSeparator name

fileName :: RawFilePath -> RawFilePath
fileName name
  | isRelative name = panic "fileName: file name must be absolute"
  | hasTrailingPathSeparator name = dropTrailingPathSeparator name
  | otherwise = name

-- NineError message
-- TODO change all the undefineds to return the old context and a
noneDetails :: Details Context
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
  }

none :: FSItem Context
none = FSItem None noneDetails []

-- fileOpen :: Fid -> Mode -> s -> (Either NineError Qid, s)
-- fileOpen _ _ context = (Left (ENotImplemented "fileOpen"), context)
-- fileWalk :: NineError
-- fileWalk = ENotADir
-- fileRead :: Fid -> Offset -> Length -> s -> (Either NineError B.ByteString, s)
-- fileRead _ _ _ context = (Left (ENotImplemented "fileOpen"), context)
-- fileWrite :: Fid -> Offset -> ByteString -> FidState -> FSItem s -> s -> IO (Either NineError Count, s)
-- fileWrite fid offset bs (FidState _ i c)
-- fileWrite _ _ _ context = (Left (ENotImplemented "fileOpen"), context)
fdClunk :: Fid -> FSItem Context -> Context -> (Maybe NineError, Context)
fdClunk fid _ c = (Nothing, c {cFids = HashMap.delete fid (cFids c)})

-- fileFlush :: s -> s
-- fileFlush context = context
dirAttach
  :: Fid
  -> AFid
  -> UserName
  -> AccessName
  -> FSItemsIndex
  -> FSItem Context
  -> Context
  -> (Either NineError Qid, Context)
dirAttach fid _ _ _ i d c =
  ( Right (Qid [Qid.Directory] ((dVersion . fDetails) d) (fromIntegral i))
  , c {cFids = HashMap.insert fid (FidState Nothing i) (cFids c)})

fileAttach
  :: Fid
  -> AFid
  -> UserName
  -> AccessName
  -> FSItemsIndex
  -> FSItem Context
  -> Context
  -> (Either NineError Qid, Context)
fileAttach _ _ _ _ _ _ c =
  (Left (OtherError "fileAttach: file cannot be attached"), c)

fdCreate
  :: Fid
  -> ByteString
  -> Permissions
  -> Mode
  -> FSItem Context
  -> Context
  -> (Either NineError (Qid, IOUnit), Context)
fdCreate _ _ _ _ _ context = (Left (ENotImplemented "fileCreate"), context)

-- TODO check for permissions, etc
-- TODO bug creating fid should happen in walk, not here
-- TODO when opened for reading, create and add the TQueue here
fileOpen
  :: Fid
  -> Mode
  -> FidState
  -> FSItem Context
  -> Context
  -> IO (Either NineError (Qid, IOUnit), Context)
fileOpen fid mode fidState me c
  | mode == 0 && isJust (fidQueue fidState) -- OREAD and Q already exists
   =
    return
      ( Right
          ( Qid
              [Qid.File]
              ((dVersion . fDetails) me)
              (fidFSItemsIndex fidState)
          , iounit)
      , c)
  | mode == 0 -- OREAD
   = do
    readQ <- newTQueueIO
    return
      ( Right
          ( Qid
              [Qid.File]
              ((dVersion . fDetails) me)
              (fidFSItemsIndex fidState)
          , iounit)
      , c
        { cFids =
            HashMap.insert fid (fidState {fidQueue = Just readQ}) (cFids c)
        })
  | otherwise =
    return
      ( Right
          ( Qid
              [Qid.File]
              ((dVersion . fDetails) me)
              (fidFSItemsIndex fidState)
          , iounit)
      , c)
  where
    iounit = fromIntegral ((cMaxMessageSize c) - 23) -- maximum size of each message

-- TODO check for permissions, etc
-- TODO bug creating fid should happen in walk, not here
-- TODO when opened for reading, create and add the TQueue here
dirOpen
  :: Fid
  -> Mode
  -> FidState
  -> FSItem Context
  -> Context
  -> IO (Either NineError (Qid, IOUnit), Context)
dirOpen fid _ fidState me c =
  let iounit = fromIntegral ((cMaxMessageSize c) - 23) -- maximum size of each message
  in return
       ( Right
           ( Qid
               [Qid.Directory]
               ((dVersion . fDetails) me)
               (fidFSItemsIndex fidState)
           , iounit)
       , c
         {cFids = HashMap.insert fid (fidState {fidQueue = Nothing}) (cFids c)})

fileWrite
  :: Fid
  -> Offset
  -> ByteString
  -> FidState
  -> FSItem s
  -> s
  -> IO (Either NineError Count, s)
fileWrite _ _ _ (FidState Nothing _) _ c =
  return ((Left . OtherError) "No Queue to read from", c)
fileWrite fid offset bs fs@(FidState (Just q) i) me c = do
  atomically (writeTQueue q bs)
  return ((Right . fromIntegral . BS.length) bs, c)

-- TODO check for permissions, iounit details, etc
-- TODO ignoring offset and count
fileRead
  :: Fid
  -> Offset
  -> Count
  -> FidState
  -> FSItem Context
  -> Context
  -> IO (Either NineError ByteString)
fileRead _ _ _ (FidState Nothing _) _ c =
  return ((Left . OtherError) "No Queue to read from")
fileRead _ _ _ (FidState (Just q) _) _ _ =
  (fmap Right . atomically . readTQueue) q

-- TODO check for permissions, iounit details, etc
dirRead
  :: Fid
  -> Offset
  -> Count
  -> FidState
  -> FSItem Context
  -> Context
  -> IO (Either NineError ByteString)
dirRead _ _ _ _ fsItem c =
  let fsItems = cFSItems c
      -- remove the trailing slash of the directory
      dirname = (takeDirectory . stName . dStat . fDetails) fsItem
      childrenStats =
        (V.map (dStat . fDetails) . V.filter (belongsToDir dirname)) fsItems
      childrenStatsBS = V.map (runPut . put) childrenStats
  in (return . Right . BS.concat . V.toList) childrenStatsBS

belongsToDir :: RawFilePath -> FSItem Context -> Bool
belongsToDir fp fsItem
  | fType fsItem == Directory =
    fp == (takeDirectory . takeDirectory . stName . dStat . fDetails) fsItem
  | fType fsItem == File =
    fp == (takeDirectory . stName . dStat . fDetails) fsItem
  | otherwise = False

-- TODO http://man2.aiju.de/5/remove -- What is the behaviour if the concerned fid is a directory? remove the directory? how about any files in that directory?  [20:34]
fileRemove :: Fid
           -> FidState
           -> FSItem Context
           -> Context
           -> (Maybe NineError, Context)
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
-- TODO allow more changes to stat as per the spec
writeStat
  :: Fid
  -> Stat
  -> FidState
  -> FSItem Context
  -> Context
  -> (Maybe NineError, Context)
writeStat _ stat fidState me c =
  let oldstat = (dStat . fDetails) me
      updatedStat =
        oldstat
        -- stTyp    = !Word16
        -- , stDev    = !Word32
        -- , stQid    = stQid stat
        -- , stMode   = stMode stat
        --                         , stAtime  = !Word32
        { stMtime = stMtime stat
          --                         , stLength = !Word64
          --                         , stName   = !ByteString
          --                         , stUid    = !ByteString
          --                         , stGid    = !ByteString
          --                         , stMuid   = !ByteString
        }
      newFSItem = me {fDetails = (fDetails me) {dStat = updatedStat}}
      index = fidFSItemsIndex fidState
      updatedContext =
        c {cFSItems = V.modify (\v -> DVM.write v index newFSItem) (cFSItems c)}
  in (Nothing, updatedContext)

dirStat, fileStat :: FSItemsIndex -> Stat
dirStat = initialStat Qid.Directory

fileStat = initialStat Qid.File

initialStat :: QType -> FSItemsIndex -> Stat
initialStat Qid.Directory index =
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
initialStat Qid.File index =
  Stat
  { stTyp = 0
  , stDev = 0
  , stQid = Qid [Qid.File, Qid.AppendOnlyFile] 0 index
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
initialStat _ index = nullStat index

nullStat :: FSItemsIndex -> Stat
nullStat index =
  Stat
  { stTyp = 0
  , stDev = 0
  , stQid = Qid [Qid.AppendOnlyFile] 0 index
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
fdWalk
  :: Fid
  -> NewFid
  -> [ByteString]
  -> FidState
  -> FSItem Context
  -> Context
  -> (Either NineError [Qid], Context)
fdWalk _ newfid [] fidState _ c =
  ( Right []
  , c {cFids = HashMap.insert newfid (fidState {fidQueue = Nothing}) (cFids c)})
fdWalk fid newfid nwnames _ d c
  | V.length fsItems > 1 =
    ( Left
        (OtherError
           (BS.append
              "dirWalk: Multiple FSItems with the same name found"
              (joinPath nwnames)))
    , c)
  | V.length fsItems == 0 =
    let qids =
          traceShowId
            ((catMaybes .
              takeWhile isJust .
              fmap (buildQid (cFSItems c)) . scanl' combine fidName)
               nwnames)
    in (Right qids, c)
  | otherwise =
    let qids =
          traceShowId
            ((catMaybes . fmap (buildQid (cFSItems c)) . scanl' combine fidName)
               nwnames)
    in ( Right qids
       , c
         { cFids =
             HashMap.insert newfid (FidState Nothing fsItemIndex) (cFids c)
         })
  where
    fidName = (stName . dStat . fDetails) d
    fsItems =
      traceShowId
        (V.findIndices
           (hasName (traceShowId (combine fidName (joinPath nwnames))))
           (cFSItems c))
    fsItemIndex = V.head (traceShowId fsItems)

--     fsItem = fromMaybe d ((cFSItems c) V.!? fsItemIndex)
-- convert of "//" to "/"
normalizePath :: RawFilePath -> RawFilePath
normalizePath = joinPath . splitDirectories

buildQid :: Vector (FSItem Context) -> RawFilePath -> Maybe Qid
buildQid fsItems path =
  V.findIndex (hasName path) fsItems >>=
  (\fsItemIndex ->
     (fsItems V.!? fsItemIndex) >>= \fsitem ->
       Just
         (Qid
            [fsItemToQType fsitem]
            ((dVersion . fDetails) fsitem)
            (fromIntegral fsItemIndex)))

hasName :: RawFilePath -> FSItem Context -> Bool
hasName name fsitem =
  let normalizedName = normalizePath name
  in normalizedName == (normalizePath . stName . dStat . fDetails) fsitem

-- dirWalk
--   :: Fid
--   -> NewFid
--   -> [ByteString]
--   -> FidState
--   -> FSItem Context
--   -> Context
--   -> (Either NineError [Qid], Context)
-- -- dirWalk fid newfid [] fidState d c = (Right [],c)
-- dirWalk fid newfid newNames fidState d c
--   | V.length fsItems > 1 =
--     ( Left
--         (OtherError
--            (BS.append
--               "dirWalk: Multiple FSItems with the same name found"
--               (joinPath nwnames)))
--     , c)
--   | V.length fsItems == 0 =
--     let qids =
--           (catMaybes . fmap (buildQid (cFSItems c)) . scanl combine fidName)
--             nwnames
--     in (Right qids, c)
--   | otherwise =
--     let qids =
--           (catMaybes . fmap (buildQid (cFSItems c)) . scanl combine fidName)
--             nwnames
--     in ( Right qids
--        , c
--          { cFids =
--              HashMap.insert newfid (FidState Nothing fsItemIndex) (cFids c)
--          })
--   where
--     fidName = (stName . dStat . fDetails) d
--     fsItems =
--       V.findIndices (hasName (combine fidName (joinPath nwnames))) (cFSItems c)
--     fsItemIndex = V.head fsItems
--     fsItem = fromMaybe d ((cFSItems c) V.!? fsItemIndex)
-- when length qids == length nwnames = insert newfid

-- fileWalk2
--   :: Fid
--   -> ByteString
--   -> [Qid]
--   -> [ByteString]
--   -> FSItemsIndex
--   -> FSItem Context
--   -> Context
--   -> (Either NineError [Qid], Context)
-- fileWalk2 newfid name parentQids _ fsItemIndex fsItem c =
--   ( Right
--       (parentQids ++
--        [ Qid
--            ((qType . stQid . dStat . fDetails) fsItem)
--            ((dVersion . fDetails) fsItem)
--            fsItemIndex
--        ])
--   , c {cFids = HashMap.insert newfid (FidState Nothing fsItemIndex) (cFids c)})

-- dirWalk2
--   :: Fid
--   -> ByteString
--   -> [Qid]
--   -> [ByteString]
--   -> FSItemsIndex
--   -> FSItem Context
--   -> Context
--   -> (Either NineError [Qid], Context)
-- dirWalk2 newfid name parentQids [] fsItemIndex fsItem c =
--   ( Right
--       (parentQids ++
--        [ Qid
--            ((qType . stQid . dStat . fDetails) fsItem)
--            ((dVersion . fDetails) fsItem)
--            fsItemIndex
--        ])
--   , c {cFids = HashMap.insert newfid (FidState Nothing fsItemIndex) (cFids c)})
-- dirWalk2 newfid name parentQids (f:fs) fsItemIndex fsItem c =
--   let vectorFSItems = cFSItems c
--       newName = combine (normalizePath name) f
--   in case V.findIndex (hasName newName) vectorFSItems of
--        Nothing -> (Right parentQids, c)
--        (Just fsItemIndex) ->
--          case vectorFSItems V.!? fsItemIndex of
--            Nothing -> (Right parentQids, c)
--            (Just fsItem) ->
--                     ((dWalk . fDetails) fsItem)
--                     newfid
--                     newName
--                     (parentQids ++
--                         [ Qid
--                             ((qType . stQid . dStat . fDetails) fsItem)
--                             ((dVersion . fDetails) fsItem)
--                             fsItemIndex
--                         ])
--                     fs
--                     fsItemIndex
--                     fsItem
--                     c

fileWalk
  :: NewFid
  -> RawFilePath
  -> [Qid] -- parent qids
  -> [RawFilePath] -- still to traverse
  -> Context
  -> (Either NineError [Qid], Context)
fileWalk newfid name parentQids [] c =
  case (findIndexUsingName name (cFSItems c)) of
    Nothing -> ( Right parentQids , c )
    Just fsItemsIndex ->
        ( Right parentQids
        , c {cFids = HashMap.insert newfid (FidState Nothing fsItemsIndex) (cFids c)})

findIndexUsingName :: RawFilePath -> Vector (FSItem Context) -> Maybe Int
findIndexUsingName name = V.findIndex (hasName name)

dirWalk
  :: NewFid
  -> RawFilePath
  -> [Qid] -- parent qids
  -> [RawFilePath] -- still to traverse
  -> Context
  -> (Either NineError [Qid], Context)
dirWalk newfid name parentQids [] c =
  case (findIndexUsingName name (cFSItems c)) of
    Nothing -> ( Right parentQids , c )
    Just fsItemsIndex ->
        ( Right parentQids
        , c {cFids = HashMap.insert newfid (FidState Nothing fsItemsIndex) (cFids c)})

dirWalk newfid name parentQids (f:fs) c =
  case (findIndexUsingName (combine name f) (cFSItems c)) of
    Nothing -> ( Right parentQids , c )
    Just fsItemsIndex ->
      case (cFSItems c) V.!? fsItemsIndex of
        Nothing -> ( Right parentQids , c )
        Just fsItem ->
                    ((dWalk . fDetails) fsItem)
                    newfid
                    (combine name f)
                    (parentQids ++
                        [ Qid
                            ((qType . stQid . dStat . fDetails) fsItem)
                            ((dVersion . fDetails) fsItem)
                            fsItemsIndex
                        ])
                    fs
                    c
