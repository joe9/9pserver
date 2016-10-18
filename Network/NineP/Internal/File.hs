{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Network.NineP.Internal.File where

import Protolude
import Data.ByteString (ByteString)
import           Data.Default
import           Data.Text                  (Text)
import           Data.Vector                (Vector)

import Data.NineP
import Network.NineP.Error

data FSItemType = Directory | File | None

data FSItem s = FSItem
    { fType :: FSItemType
    , fDetails :: Details s
    , fChildren :: Maybe (Vector (FSItem s))}

type IOUnit = Word32

data Details s = Details
  { dOpen :: Fid -> Mode -> FSItem s -> s -> (Either NineError (Qid,IOUnit), s)
  , dWalk :: Fid -> NewFid -> [Text] -> FSItem s -> s -> (Either NineError [Qid], s)
  , dRead :: Fid -> Offset -> Length -> FSItem s -> s -> (Either NineError ByteString, s)
  , dReadStat :: Fid -> FSItem s -> s -> (Either NineError Stat, s)
  , dWriteStat :: Fid -> Stat -> FSItem s -> s -> (Maybe NineError, s)
  , dStat :: Stat
  , dWrite :: Fid -> Offset -> ByteString -> FSItem s -> s -> (Either NineError Length, s)
  , dClunk :: Fid -> FSItem s -> s -> (Maybe NineError, s)
  , dFlush :: s -> FSItem s -> s
  , dAttach :: Fid -> AFid -> UserName -> AccessName -> FSItem s -> s -> (Either NineError Qid, s)
  , dCreate :: Fid -> ByteString -> Permissions -> Mode -> FSItem s -> s -> (Either NineError (Qid,IOUnit), s)
  , dRemove :: Fid -> FSItem s -> s -> (Maybe NineError, s)
  , dVersion :: Word32
  }

-- fileDetails
--   =
--     Details
--     { dOpen = fileOpen
--     , dWalk = undefined -- fileWalk
--     , dRead = fileRead
--     , dStat = nullStat
--     , dWrite = fileWrite
--     , dClunk = fileClunk
--     , dFlush = fileFlush
--     , dAttach = fileAttach
--     , dCreate = fileCreate
--     , dRemove = fileRemove
--     , dVersion = 0
--     }

-- --         ,fFreefid = fileFreefid
-- dirDetails =
--     Details
--     { dOpen = fileOpen
--     , dWalk = dirWalk
--     , dRead = fileRead
--     , dStat = nullStat
--     , dWrite = fileWrite
--     , dClunk = fileClunk
--     , dFlush = fileFlush
--     , dAttach = fileAttach
--     , dCreate = fileCreate
--     , dRemove = fileRemove
--   , dVersion = 0
--     }

-- -- TODO below functions
-- noneDetails =
--     Details
--     { dOpen = fileOpen
--     , dWalk = dirWalk
--     , dRead = fileRead
--     , dStat = nullStat
--     , dWrite = fileWrite
--     , dClunk = fileClunk
--     , dFlush = fileFlush
--     , dAttach = fileAttach
--     , dCreate = fileCreate
--     , dRemove = fileRemove
--   , dVersion = 0
--     }

-- fileOpen :: Fid -> Mode -> s -> (Either NineError Qid, s)
-- fileOpen _ _ context = (Left (ENotImplemented "fileOpen"), context)

-- fileWalk :: NineError
-- fileWalk = ENotADir

-- fileRead :: Fid -> Offset -> Length -> s -> (Either NineError B.ByteString, s)
-- fileRead _ _ _ context = (Left (ENotImplemented "fileOpen"), context)

-- fileWrite :: Fid -> Offset -> B.ByteString -> s -> (Either NineError Length, s)
-- fileWrite _ _ _ context = (Left (ENotImplemented "fileOpen"), context)

-- fileClunk :: Fid -> s -> (Maybe NineError, s)
-- fileClunk _ context = (Just (ENotImplemented "fileOpen"), context)

-- fileFlush :: s -> s
-- fileFlush context = context

-- fileAttach :: Fid
--            -> AFid
--            -> UserName
--            -> AccessName
--            -> s
--            -> (Either NineError Qid, s)
-- fileAttach _ _ _ _ context = (Left (ENotImplemented "fileOpen"), context)

-- fileCreate :: Fid
--            -> Text
--            -> Permissions
--            -> Mode
--            -> s
--            -> (Either NineError Qid, s)
-- fileCreate _ _ _ _ context = (Left (ENotImplemented "fileOpen"), context)

-- fileRemove :: Fid -> s -> s
-- fileRemove _ context = context

-- nullStat :: Stat
-- nullStat =
--   Stat
--   { stTyp = 0
--   , stDev = 0
--   , stQid = Qid [] 0 0
--   , stMode = 0
--   , stAtime = 0
--   , stMtime = 0
--   , stLength = 0
--   , stName = ""
--   , stUid = ""
--   , stGid = ""
--   , stMuid = ""
--   }

-- dirWalk :: Fid -> NewFid -> [Text] -> s -> (Either NineError [Qid], s)
-- dirWalk _ _ _ context = (Left (ENotImplemented "fileOpen"), context)
