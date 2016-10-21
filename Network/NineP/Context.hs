{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.NineP.Context where

import           Control.Concurrent.STM.TQueue
import qualified Data.ByteString                  as BS
import           Data.HashMap.Strict              as HashMap
import           Data.List
import           Data.String.Conversions
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as V
import qualified Data.Vector.Mutable              as DVM
import           GHC.Show
import           Protolude
import           System.Posix.ByteString.FilePath
import           System.Posix.FilePath
import           Text.Groom

import           Data.NineP hiding (Directory, File)
import qualified Data.NineP as NineP

import Network.NineP.Error

data NineVersion
  = VerUnknown
  | Ver9P2000
  deriving (Eq)

showNineVersion :: NineVersion -> ByteString
showNineVersion Ver9P2000  = "9P2000"
showNineVersion VerUnknown = "unknown"

validateNineVersion :: ByteString -> NineVersion
validateNineVersion s =
  if BS.isPrefixOf "9P2000" s
    then Ver9P2000
    else VerUnknown

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
data FSItemType
  = Directory
  | File
  | None
  deriving (Eq, Show)

data FSItem s = FSItem
  { fType     :: FSItemType
  , fDetails  :: Details s
  , fOpenFids :: [Fid]
  }

instance Show (FSItem s) where
  show f =
    unwords
      [ groom (fType f)
      , (groom . dVersion . fDetails) f
      , (groom . dStat . fDetails) f
      , groom (fOpenFids f)
      ]

data FidState = FidState
  { fidQueue        :: Maybe (TQueue ByteString)
  , fidFSItemsIndex :: FSItemsIndex
  }

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

type FSItemsIndex = Int

-- TODO add name
data Details s = Details
  { dOpen :: Fid -> Mode -> FidState -> FSItem s -> s -> IO (Either NineError (Qid, IOUnit), s)
  , dWalk :: Fid -> NewFid -> [ByteString] -> FidState -> FSItem s -> s -> (Either NineError [Qid], s)
  , dRead :: Fid -> Offset -> Count -> FidState -> FSItem s -> s -> IO (Either NineError ByteString)
  , dReadStat :: Fid -> FSItem s -> s -> (Either NineError Stat, s)
  , dWriteStat :: Fid -> Stat -> FidState -> FSItem s -> s -> (Maybe NineError, s)
  , dStat :: Stat
  , dWrite :: Fid -> Offset -> ByteString -> FidState -> FSItem s -> s -> IO (Either NineError Count, s)
  , dClunk :: Fid -> FSItem s -> s -> (Maybe NineError, s)
  , dFlush :: FSItem s -> s -> s
  , dAttach :: Fid -> AFid -> UserName -> AccessName -> FSItemsIndex -> FSItem s -> s -> (Either NineError Qid, s)
  , dCreate :: Fid -> ByteString -> Permissions -> Mode -> FSItem s -> s -> (Either NineError (Qid, IOUnit), s)
  , dRemove :: Fid -> FidState -> FSItem s -> s -> (Maybe NineError, s)
  , dVersion :: Word32
  }

indexToQPath :: FidState -> Word64
indexToQPath = fromIntegral . fidFSItemsIndex

fsItemToQType :: FSItem Context -> QType
fsItemToQType fsitem
  | fType fsitem == Directory = NineP.Directory
  | fType fsitem == File = NineP.File
  | otherwise = NineP.File

-- TODO : Add to FileSystem
initializeContext :: Context
initializeContext = Context HashMap.empty V.empty 8196 []

resetContext :: Context -> Context
resetContext c = c {cFids = HashMap.empty}

-- TODO need some validation to ensure that the parent directory exists
-- name is an absolute path
fileDetails, dirDetails :: RawFilePath -> Details Context
fileDetails name =
  Details
  { dOpen = fileOpen
  , dWalk = fdWalk
  , dRead = fileRead
  , dStat = nullStat {stName = fileName name}
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

dirDetails name =
  Details
  { dOpen = dirOpen
  , dWalk = fdWalk
  , dRead = dirRead
  , dStat = nullStat {stName = dirName name}
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
  ( Right (Qid [NineP.Directory] ((dVersion . fDetails) d) (fromIntegral i))
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
  | mode == 0 -- OREAD
   = do
    readQ <- newTQueueIO
    return
      ( Right
          ( Qid [NineP.File] ((dVersion . fDetails) me) (indexToQPath fidState)
          , iounit)
      , c
        { cFids =
            HashMap.insert fid (fidState {fidQueue = Just readQ}) (cFids c)
        })
  | otherwise =
    return
      ( Right
          ( Qid [NineP.File] ((dVersion . fDetails) me) (indexToQPath fidState)
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
               [NineP.Directory]
               ((dVersion . fDetails) me)
               (indexToQPath fidState)
           , iounit)
       , c)

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
fileRead fid offset _ fs@(FidState (Just q) i) me c =
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
dirRead fid offset count i me c = undefined

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

-- TODO implement ".." - walk to the parent directory
fdWalk
  :: Fid
  -> NewFid
  -> [ByteString]
  -> FidState
  -> FSItem Context
  -> Context
  -> (Either NineError [Qid], Context)
fdWalk fid newfid nwnames fidState d c
  | V.length fsItems > 1 =
    ( Left
        (OtherError
           (BS.append
              "dirWalk: Multiple FSItems with the same name found"
              (joinPath nwnames)))
    , c)
  | V.length fsItems == 0 =
    let qids =
          (catMaybes . fmap (buildQid (cFSItems c)) . scanl combine fidName)
            nwnames
    in (Right qids, c)
  | otherwise =
    let qids =
          (catMaybes . fmap (buildQid (cFSItems c)) . scanl combine fidName)
            nwnames
    in ( Right qids
       , c
         { cFids =
             HashMap.insert newfid (FidState Nothing fsItemIndex) (cFids c)
         })
  where
    fidName = (stName . dStat . fDetails) d
    fsItems =
      V.findIndices (hasName (combine fidName (joinPath nwnames))) (cFSItems c)
    fsItemIndex = V.head fsItems
    fsItem = fromMaybe d ((cFSItems c) V.!? fsItemIndex)

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
  let normalizedName = joinPath (splitDirectories name)
  in normalizedName == (stName . dStat . fDetails) fsitem ||
     (BS.append normalizedName "/") == (stName . dStat . fDetails) fsitem
