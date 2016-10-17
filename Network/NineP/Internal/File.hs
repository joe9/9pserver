{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Network.NineP.Internal.File where

import Protolude
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Default
import           Data.Text                  (Text)
import           Data.Vector                (Vector)

import Data.NineP
import Network.NineP.Error

data FileDetails s = FileDetails
  { fOpen :: Fid -> Mode -> s -> (Either NineError Qid, s)
  , fWalk :: NineError
  , fRead :: Fid -> Offset -> Length -> s -> (Either NineError B.ByteString, s)
  , fStat :: Stat
  , fWrite :: Fid -> Offset -> B.ByteString -> s -> (Either NineError Length, s)
  , fClunk :: Fid -> s -> (Maybe NineError, s)
  , fFlush :: s -> s
  , fAttach :: Fid -> AFid -> UserName -> AccessName -> s -> (Either NineError Qid, s)
  , fCreate :: Fid -> Text -> Permissions -> Mode -> s -> (Either NineError Qid, s)
  , fRemove :: Fid -> s -> s
  , fVersion :: Word32
  }

instance Default (FileDetails s) where
  def =
    FileDetails
    { fOpen = fileOpen
    , fWalk = fileWalk
    , fRead = fileRead
    , fStat = nullStat
    , fWrite = fileWrite
    , fClunk = fileClunk
    , fFlush = fileFlush
    , fAttach = fileAttach
    , fCreate = fileCreate
    , fRemove = fileRemove
  , fVersion = 0
    }

--         ,fFreefid = fileFreefid
data DirDetails s = DirDetails
  { dOpen :: Fid -> Mode -> s -> (Either NineError Qid, s)
  , dWalk :: Fid -> NewFid -> [Text] -> s -> (Either NineError [Qid], s)
  , dRead :: Fid -> Offset -> Length -> s -> (Either NineError B.ByteString, s)
  , dStat :: Stat
  , dWrite :: Fid -> Offset -> B.ByteString -> s -> (Either NineError Length, s)
  , dClunk :: Fid -> s -> (Maybe NineError, s)
  , dFlush :: s -> s
  , dAttach :: Fid -> AFid -> UserName -> AccessName -> s -> (Either NineError Qid, s)
  , dCreate :: Fid -> Text -> Permissions -> Mode -> s -> (Either NineError Qid, s)
  , dRemove :: Fid -> s -> s
  , dVersion :: Word32
  }

instance Default (DirDetails s) where
  def =
    DirDetails
    { dOpen = fileOpen
    , dWalk = dirWalk
    , dRead = fileRead
    , dStat = nullStat
    , dWrite = fileWrite
    , dClunk = fileClunk
    , dFlush = fileFlush
    , dAttach = fileAttach
    , dCreate = fileCreate
    , dRemove = fileRemove
  , dVersion = 0
    }

data FSItem s
  = File (FileDetails s)
  | Dir (DirDetails s)
        (Vector (FSItem s))
  | Free

fileOpen :: Fid -> Mode -> s -> (Either NineError Qid, s)
fileOpen _ _ context = (Left (ENotImplemented "fileOpen"), context)

fileWalk :: NineError
fileWalk = ENotADir

fileRead :: Fid -> Offset -> Length -> s -> (Either NineError B.ByteString, s)
fileRead _ _ _ context = (Left (ENotImplemented "fileOpen"), context)

fileWrite :: Fid -> Offset -> B.ByteString -> s -> (Either NineError Length, s)
fileWrite _ _ _ context = (Left (ENotImplemented "fileOpen"), context)

fileClunk :: Fid -> s -> (Maybe NineError, s)
fileClunk _ context = (Just (ENotImplemented "fileOpen"), context)

fileFlush :: s -> s
fileFlush context = context

fileAttach :: Fid
           -> AFid
           -> UserName
           -> AccessName
           -> s
           -> (Either NineError Qid, s)
fileAttach _ _ _ _ context = (Left (ENotImplemented "fileOpen"), context)

fileCreate :: Fid
           -> Text
           -> Permissions
           -> Mode
           -> s
           -> (Either NineError Qid, s)
fileCreate _ _ _ _ context = (Left (ENotImplemented "fileOpen"), context)

fileRemove :: Fid -> s -> s
fileRemove _ context = context

nullStat :: Stat
nullStat =
  Stat
  { stTyp = 0
  , stDev = 0
  , stQid = Qid [] 0 0
  , stMode = 0
  , stAtime = 0
  , stMtime = 0
  , stLength = 0
  , stName = ""
  , stUid = ""
  , stGid = ""
  , stMuid = ""
  }

dirWalk :: Fid -> NewFid -> [Text] -> s -> (Either NineError [Qid], s)
dirWalk _ _ _ context = (Left (ENotImplemented "fileOpen"), context)
