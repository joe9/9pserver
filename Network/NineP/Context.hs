{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.NineP.Context where

import           Control.Concurrent.STM.TQueue
import qualified Data.ByteString                  as BS
import           Data.Default
import           Data.HashMap.Strict              as HashMap
import           Data.List
import           Data.Serialize
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as V
import qualified Data.Vector.Mutable              as DVM
import qualified GHC.Show                         as Show
import           Protolude                        hiding (put)
import           System.Posix.ByteString.FilePath
import           System.Posix.FilePath
import           Text.Groom

import           Data.NineP
import           Data.NineP.Qid
import qualified Data.NineP.Qid  as Qid
import           Data.NineP.Stat
import qualified Data.NineP.Stat as Stat

import Network.NineP.Error

-- <joe9> I am not storing any data in the filesystem. Even if I do, it is not more than a line.
-- <joe9> I am implementing a 9P server. This entails serving an in-memory filesystem (synthetic filesystem)  [21:04]
-- <joe9> https://gist.github.com/60b4154eb32a00e265abe302d6a70a5d as an fyi, this is the relevant C code of another implementation.
-- <joe9> The directory structure of the file system is pretty rudimentary. No actual data is stored in any physical file.  [21:05]
-- <joe9> It is like fuse where you provide a list of functions for file read, write, create, open, etc.  [21:06]
-- <joe9> and the fuse server executes the relevant function based on the syscall.
-- <joe9> this particular implementation, stores a list of FileDescriptors provided. (when open is called, it provides them a file descriptor)
-- <joe9> and, keeps the file descriptor until it is told to remove it.  [21:07]
-- <joe9> Each file/dir in this structure is indexed with a Word64  [21:08]
-- <joe9> Basically, a zipper data structure (file system, the zipper FSCrumb filesystem described in LYAH is good enough)
-- <joe9> and a vector of Index to every object in the above file system
-- <joe9> and another list of open filehandles (cross-refernced to the above index in the vector)  [21:09]
-- <joe9> 3 data structures in total
-- <geekosaur> zipper doesn't seem right for this unless there is exactly one open fd at a time, to be honest
-- <joe9> that argument makes sense.  [21:10]
-- <geekosaur> (also, I have not read LYAH; I predate it, Haskell-wise, by some years...)
-- <joe9> the C implementation is maintaining a big list of lists for the file system structure https://paste.pound-python.org/show/UTUUo1Tp5TWpXkYSnPS4/  [21:11]
-- <joe9> it just seems wrong to be doing that (atleast in haskell)
-- <joe9> Is there an indexed tree ?  [21:12]
-- <joe9> that can maintain the indexes instead of us maintaining the index of each object in the tree.
-- <joe9> it should not be called index, I guess some id  [21:13]
-- <joe9> (Int identifier, (Dir | File)) -- tree tuple
-- <geekosaur> not in the standard containers library. the closest thing I can think of on hackage is correct structure-wise but not semantically (priority search queues); but that
--             doesn't mean there isn't something I haven't had any reasin to look up
-- <joe9> ok, Thanks. will check on priority search queues  [21:14]
-- <geekosaur> no
-- <geekosaur> don;t
-- <geekosaur> unles syou want to steal the structure but rewrite all the logic, which will be completely wrong for what you want
-- <joe9> How would you structure this data, if you were doing this? just Vector of ((File|Dir), parent) -- or Vector (file/directory path)  [21:16]
-- <geekosaur> I'd just use a Vector for the inode map, with the vital statistics for the filesystem entity (f->tab in the C code) in it, and one of the things in it is an ADT with
--             File and Dir selectors; the File one contains the access functions as a record, the Dir one is a list or Vector of [name, inumber]  [21:17]
-- <joe9> or, Vector (File/directory path + Name) i man
-- <geekosaur> and treat the first entry in the inode map as the root directory entry  [21:18]
-- <joe9> with a Vector, when you delete an entry, the rest of the stuff move up. That would mess up the id's, correct?  [21:19]
-- <joe9> It would have to be an IntMap or HashMap?
-- <geekosaur> correct, so you would have to have a third thing in that ADT for "unallocated", and look for those entries if yu create a new one. this only for the inode table; for
--             directory vectors the delete behavior is what you want
-- <geekosaur> you don't want to use list for either of these if you are removing things, because that's where linked lists become painful  [21:20]
-- <joe9> What do you mean by "directory vectors"?  [21:21]
-- <geekosaur> "one of the things in it is an ADT with File and Dir selectors; the File one contains the access functions as a record, the Dir one is a list or Vector of [name, inumber]"
-- <geekosaur> which I said earlier
-- <joe9> oh, ok. Thanks. Sorry, I missed that.
-- <geekosaur> so now it is File {operation record} | Dir DirVector | Free  [21:22]
-- <joe9> yes, a Vector of the above ADT
-- <geekosaur> and you can replace any Free with a new File or Dir when creating a new filesystem entity
-- <joe9> and, there would be another IntMap of the FileHandles, correct?  [21:23]
-- <joe9> one quirk is that the file handle numbers are provided by the client
-- <joe9> they are not assigned by us (the server). We just use the one provided.
-- <geekosaur> that would be IntMap or HashMap, yes
-- <joe9> They are integers always. So, IntMap then.
-- <geekosaur> that seems unfortunate; clients that don't keep proper track can mess up the server :/  [21:24]
-- <joe9> your argument makes sense.  [21:27]
-- <joe9> What would you think of also indexing with the name (Hash of the file name or directory name)?  [21:28]
-- <joe9> to check if the filename or directory name is being used or not?
-- <joe9> instead of looping over the vector to check that
-- <joe9> overkill?
-- <geekosaur> depends on how much activity you expect. for small things like this I think the overhead would be too high.
-- <geekosaur> for production filesystems on multitasking systems, this is more or less the dentry cache that linux and solaris use  [21:29]
-- <geekosaur> but keep in mind they are also factoring in things you don't need to worry about like disk read latency :)
-- <joe9> makes sense. It is also easier to add that if the need arises.  [21:30]
-- <joe9> Thanks a lot . Your input was very helpful.  [21:31]
-- <joe9> geekosaur: https://paste.pound-python.org/show/Bd5NGkjAwxeoYWcEt4Er/ is what I have come up with  [21:51]
-- <joe9> geekosaur: as an fyi, http://dpaste.com/0MW2161 is what I ended up with. Thanks.  [08:56]
-- <joe9> geekosaur: for finding a file/dir, I would have to go through the whole vector of cFSItems, correct?  [08:57]
-- <geekosaur> yes. this is going to be true of any structure  [08:58]
-- <joe9> geekosaur: http://dpaste.com/3NDWKQK is the relevant file ADT
-- <geekosaur> (and, if you are doing it often, this is where a dcache-like thing might be handy)

-- if an item is removed, switch the Used to False.
-- this is to avoid vector indexes getting changed when a delete happens
data Occupied = Occupied | Vacant deriving (Eq, Show)

data FSItem s = FSItem
  { fOccupied :: Occupied
  , fDetails  :: Details s
  , fOpenFids :: [Fid]
  }

instance Show (FSItem s) where
  show f =
    unwords
      [ (groom . fOccupied) f
      , (groom . dVersion . fDetails) f
      , (groom . dStat . fDetails) f
      , groom (fOpenFids f)
      ]

type FSItemsIndex = Int

data FidState = FidState
  { fidQueue        :: Maybe (TQueue ByteString)
  , fidFSItemsIndex :: FSItemsIndex
  } deriving (Eq)

instance Show FidState where
  show = show . fidFSItemsIndex

data BlockedRead = BlockedRead
  { bTag   :: Tag
  , bAsync :: Async Tag
  }

data Context = Context
  { cFids           :: HashMap.HashMap Fid FidState
    -- similar to an inode map,
    -- representing the filesystem tree, with the root being the 0 always
  , cFSItems        :: Vector (FSItem Context)
  , cMaxMessageSize :: Int
  , cBlockedReads   :: [BlockedRead]
  }

type IOUnit = Word32

-- TODO add name
data Details s = Details
  { dOpen :: Fid -> OpenMode -> FidState -> FSItem s -> s -> IO (Either NineError (Qid, IOUnit), s)
    --   , dWalk :: Fid -> NewFid -> [ByteString] -> FidState -> FSItem s -> s -> (Either NineError [Qid], s)
  , dWalk :: NewFid -> RawFilePath -> [Qid] -> [RawFilePath] -> Context -> (Either NineError [Qid], Context)
  , dRead :: Fid -> Offset -> Count -> FidState -> FSItem s -> s -> IO (Either NineError ByteString)
  , dReadStat :: Fid -> FSItem s -> s -> (Either NineError Stat, s)
  , dWriteStat :: Fid -> Stat -> FidState -> FSItem s -> s -> (Maybe NineError, s)
  , dStat :: Stat
  , dWrite :: Fid -> Offset -> ByteString -> FidState -> FSItem s -> s -> IO (Either NineError Count, s)
  , dClunk :: Fid -> FSItem s -> s -> (Maybe NineError, s)
  , dFlush :: FSItem s -> s -> s
  , dAttach :: Fid -> AFid -> UserName -> AccessName -> FSItemsIndex -> FSItem s -> s -> (Either NineError Qid, s)
  , dCreate :: Fid -> ByteString -> Permissions -> OpenMode -> FSItem s -> s -> (Either NineError (Qid, IOUnit), s)
  , dRemove :: Fid -> FidState -> FSItem s -> s -> (Maybe NineError, s)
  , dVersion :: Word32
  , dAbsoluteName :: RawFilePath
  }

--   , dWalk :: NewFid -> ByteString -> [Qid] -> [ByteString] -> FSItemsIndex -> FSItem Context -> Context -> (Either NineError [Qid], Context)
stModeToQType :: FSItem Context -> [QType]
stModeToQType fsItem =
  let mode = (stMode . dStat . fDetails) fsItem
  in ((\ts -> if elem Stat.Temp mode then Qid.NonBackedUp : ts else ts)
      . (\ts -> if elem Stat.Authentication mode then Qid.Authentication : ts else ts)
      . (\ts -> if elem Stat.ExclusiveUse mode then Qid.ExclusiveUse : ts else ts)
      . (\ts -> if elem Stat.AppendOnly mode then Qid.AppendOnly : ts else ts)
      . (\ts -> if elem Stat.Directory mode then Qid.Directory : ts else ts)
     ) []

-- TODO : Add to FileSystem
instance Default Context where
  def = Context HashMap.empty V.empty 8192 []
--   def = Context HashMap.empty V.empty 512 []
--   def = Context HashMap.empty sampleFSItemsList 8192 []
