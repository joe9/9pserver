{-# LANGUAGE OverloadedStrings #-}

module Network.NineP.Internal.File where

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Default
import           Data.Text                  (Text)
import           Data.Vector                (Vector)
import           Data.Word

import Data.NineP
import Network.NineP.Error

type Offset = Word64

type Length = Word32

type ResultingData = B.ByteString

type WriteData = B.ByteString

type Fid = Word32

type AFid = Word32

type NewFid = Word32

type Permissions = Word32

type Mode = Word8

type UserName = Text

type AccessName = Text

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
    }

type Version = Word32 -- s for context including user state

data FSItem s
  = File (FileDetails s)
  | Dir (DirDetails s)
        (Vector (FSItem s))
  | Free

fileOpen :: Fid -> Mode -> s -> (Either NineError Qid, s)
fileOpen _ _ state = (Left (ENotImplemented "fileOpen"), state)

fileWalk :: NineError
fileWalk = ENotADir

fileRead :: Fid -> Offset -> Length -> s -> (Either NineError B.ByteString, s)
fileRead _ _ _ state = (Left (ENotImplemented "fileOpen"), state)

fileWrite :: Fid -> Offset -> B.ByteString -> s -> (Either NineError Length, s)
fileWrite _ _ _ state = (Left (ENotImplemented "fileOpen"), state)

fileClunk :: Fid -> s -> (Maybe NineError, s)
fileClunk _ state = (Just (ENotImplemented "fileOpen"), state)

fileFlush :: s -> s
fileFlush state = state

fileAttach :: Fid
           -> AFid
           -> UserName
           -> AccessName
           -> s
           -> (Either NineError Qid, s)
fileAttach _ _ _ _ state = (Left (ENotImplemented "fileOpen"), state)

fileCreate :: Fid
           -> Text
           -> Permissions
           -> Mode
           -> s
           -> (Either NineError Qid, s)
fileCreate _ _ _ _ state = (Left (ENotImplemented "fileOpen"), state)

fileRemove :: Fid -> s -> s
fileRemove _ state = state

nullStat :: Stat
nullStat =
  Stat
  { st_typ = 0
  , st_dev = 0
  , st_qid = Qid 0 0 0
  , st_mode = 0
  , st_atime = 0
  , st_mtime = 0
  , st_length = 0
  , st_name = ""
  , st_uid = ""
  , st_gid = ""
  , st_muid = ""
  }

dirWalk :: Fid -> NewFid -> [Text] -> s -> (Either NineError [Qid], s)
dirWalk _ _ _ state = (Left (ENotImplemented "fileOpen"), state)
