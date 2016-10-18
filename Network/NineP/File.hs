{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Network.NineP.File where

import Protolude
import Data.ByteString (ByteString)
import           Data.Default
import           qualified Data.HashMap.Strict as HashMap
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
type IndexInQids = Int

data Details s = Details
  { dOpen :: Fid -> Mode -> IndexInQids -> FSItem s -> s -> (Either NineError (Qid,IOUnit), s)
  , dWalk :: Fid -> NewFid -> [Text] -> FSItem s -> s -> (Either NineError [Qid], s)
  , dRead :: Fid -> Offset -> Length -> IndexInQids -> FSItem s -> s -> (Either NineError ByteString, s)
  , dReadStat :: Fid -> FSItem s -> s -> (Either NineError Stat, s)
  , dWriteStat :: Fid -> Stat -> IndexInQids -> FSItem s -> s -> (Maybe NineError, s)
  , dStat :: Stat
  , dWrite :: Fid -> Offset -> ByteString -> IndexInQids ->  FSItem s -> s -> (Either NineError Length, s)
  , dClunk :: Fid -> FSItem s -> s -> (Maybe NineError, s)
  , dFlush :: s -> FSItem s -> s
  , dAttach :: Fid -> AFid -> UserName -> AccessName -> Int -> FSItem s -> s -> (Either NineError Qid, s)
  , dCreate :: Fid -> ByteString -> Permissions -> Mode -> FSItem s -> s -> (Either NineError (Qid,IOUnit), s)
  , dRemove :: Fid -> Int -> FSItem s -> s -> (Maybe NineError, s)
  , dVersion :: Word32
  }
