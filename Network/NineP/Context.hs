{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.NineP.Context where

import           Control.Concurrent.STM.TQueue
import qualified Data.ByteString                  as BS
import qualified Data.HashMap.Strict              as HashMap
import           Data.IxSet.Typed                 as IxSet hiding
                                                            (flatten)
import           Data.List
import           Data.Tree
import qualified GHC.Show                         as Show
import           Protolude                        hiding (Proxy, put)
import           System.Posix.ByteString.FilePath
import           System.Posix.FilePath
import           Text.Groom

import           Data.NineP
import           Data.NineP.OpenMode
import           Data.NineP.Qid
import qualified Data.NineP.Qid      as Qid
import           Data.NineP.Stat
import qualified Data.NineP.Stat     as Stat

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
data Occupied
  = Occupied
  | Vacant
  deriving (Eq, Show, Ord, Typeable)

data FSItem s = FSItem
  { fOccupied     :: Occupied
  , fDetails      :: Details s
  , fAbsoluteName :: AbsolutePath
  , fsItemId      :: FSItemId -- primary key, used by Qid.Path
  }

newtype AbsolutePath = AbsolutePath
  { unAbsolutePath :: RawFilePath
  } deriving (Eq, Show, Ord)

newtype FSItemId =
  FSItemId Int
  deriving (Eq, Show, Ord)

type FSItemIxs = '[FSItemId, AbsolutePath, Occupied]

type IxFSItem s = IxSet FSItemIxs (FSItem s)

instance Indexable FSItemIxs (FSItem s) where
  indices =
    ixList
      (ixFun (\fsi -> [fsItemId fsi]))
      (ixFun (\fsi -> [fAbsoluteName fsi]))
      (ixFun (\fsi -> [fOccupied fsi]))

-- ideally, would prefer this to be Fid. It does not make sense to
--   have Id of an Id (Fid is itself an Id). But, Fid as per the
--   9P2000 spec is a Word32 and it gets confusing to have a different
--   type for that.
newtype FidId =
  FidId Fid
  deriving (Eq, Show, Ord)

data FidState = FidState
  { fidQueue    :: Maybe (TQueue ByteString)
  , fidResponse :: Maybe ByteString
  , fFidId      :: FidId -- primary key
  } deriving (Eq)

data FSItemFid = FSItemFid
  { ffFSItemId :: FSItemId
  , ffFid      :: FidId
  } deriving (Eq, Show, Ord)

type FSItemFidIxs = '[FSItemId, FidId]

type IxFSItemFid = IxSet FSItemFidIxs FSItemFid

instance Indexable FSItemFidIxs FSItemFid where
  indices = ixList (ixFun (\i -> [ffFSItemId i])) (ixFun (\i -> [ffFid i]))

data Context u = Context
  { cFids            :: HashMap.HashMap Fid FidState
    -- similar to an inode map,
    -- representing the filesystem tree, with the root being the 0 always
    -- TODO: Would have to change this to an IxSet to make it easier to
    --        search using dAbsoluteName and also have an index to use
    --        for Qid.Path
  , cFSItems         :: IxSet FSItemIxs (FSItem (Context u))
  , cMaxMessageSize  :: Int
  , cBlockedReads    :: [BlockedRead]
  , cUserState       :: u
  , cFSItemFids      :: IxSet FSItemFidIxs FSItemFid
  , cFSItemIdCounter :: Int
  }

getFSItemOfFid :: Fid -> Context u -> Maybe (FSItem (Context u))
getFSItemOfFid fid c =
  IxSet.getOne ((cFSItemFids c) @= FidId fid) >>=
  (\ff -> IxSet.getOne ((cFSItems c) @= ffFSItemId ff))

instance Show FidState where
  show (FidState Nothing r i) = "FidState Nothing " ++ show r ++ " " ++ show i
  show (FidState (Just _) r i) = "FidState <TQueue> " ++ show r ++ " " ++ show i

instance Show (FSItem s) where
  show f =
    unwords
      [ (groom . fsItemId) f
      , (groom . fAbsoluteName) f
      , (groom . fOccupied) f
      , (groom . dVersion . fDetails) f
      , (groom . dStat . fDetails) f
      ]

instance Eq (FSItem s) where
  a == b = (fsItemId a) == (fsItemId b)

instance Ord (FSItem s) where
  compare a b = compare (fsItemId a) (fsItemId b)

type FSItemsIndex = Int

instance Monoid AbsolutePath where
  mempty = AbsolutePath BS.empty
  mappend (AbsolutePath a) (AbsolutePath b) = AbsolutePath (combine a b)

data ReadResponse
  = ReadError ByteString
  | ReadResponse ByteString
  | ReadQ (TQueue ByteString)
          Count -- Offset Count (Async Tag)
  deriving (Eq)

instance Show ReadResponse where
  show (ReadError bs)    = "ReadError " ++ show bs
  show (ReadResponse bs) = "ReadResponse " ++ show bs
  show (ReadQ _ count)   = "ReadQ <TQueue> " ++ show count

data BlockedRead = BlockedRead
  { bTag   :: !Tag
  , bCount :: !Word32
  , bAsync :: Async Tag
  }

type IOUnit = Word32

-- TODO add name
data Details s = Details
  { dOpen :: Fid -> OpenMode -> FidState -> FSItem s -> s -> IO (Either NineError (Qid, IOUnit), s)
    --   , dWalk :: Fid -> NewFid -> [ByteString] -> FidState -> FSItem s -> s -> (Either NineError [Qid], s)
  , dWalk :: NewFid -> AbsolutePath -> [Qid] -> [RawFilePath] -> s -> (Either NineError [Qid], s)
  , dRead :: Fid -> Offset -> Count -> FidState -> FSItem s -> s -> IO (ReadResponse, s)
  , dReadStat :: Fid -> FSItem s -> s -> (Either NineError Stat, s)
  , dWriteStat :: Fid -> Stat -> FSItem s -> s -> (Maybe NineError, s)
  , dStat :: Stat
  , dWrite :: Fid -> Offset -> ByteString -> FSItem s -> s -> IO (Either NineError Count, s)
  , dClunk :: Fid -> FidState -> FSItem s -> s -> IO (Maybe NineError, s)
  , dFlush :: FSItem s -> s -> s
  , dAttach :: Fid -> AFid -> UserName -> AccessName -> FSItem s -> s -> (Either NineError Qid, s)
  , dCreate :: Fid -> ByteString -> Permissions -> OpenMode -> FSItem s -> s -> (Either NineError (Qid, IOUnit), s)
  , dRemove :: Fid -> FSItem s -> s -> (Maybe NineError, s)
  , dVersion :: Word32
  }

-- stModeToQType :: FSItem (Context u) -> [QType]
-- stModeToQType fsItem =
--   let mode = (stMode . dStat . fDetails) fsItem
--   in ((\ts ->
--          if elem Stat.Temp mode
--            then Qid.NonBackedUp : ts
--            else ts) .
--       (\ts ->
--          if elem Stat.Authentication mode
--            then Qid.Authentication : ts
--            else ts) .
--       (\ts ->
--          if elem Stat.ExclusiveUse mode
--            then Qid.ExclusiveUse : ts
--            else ts) .
--       (\ts ->
--          if elem Stat.AppendOnly mode
--            then Qid.AppendOnly : ts
--            else ts) .
--       (\ts ->
--          if elem Stat.Directory mode
--            then Qid.Directory : ts
--            else ts))
--        []
stModeToQType :: FSItem (Context u) -> [QType]
stModeToQType fsItem =
  let mode = (stMode . dStat . fDetails) fsItem
  in mapMaybe
       ((flip lookup)
          [ (Stat.Temp, Qid.NonBackedUp)
          , (Stat.Authentication, Qid.Authentication)
          , (Stat.ExclusiveUse, Qid.ExclusiveUse)
          , (Stat.AppendOnly, Qid.AppendOnly)
          , (Stat.Directory, Qid.Directory)
          ])
       mode

defaultContext :: u -> Context u
defaultContext userState =
  Context HashMap.empty IxSet.empty 8192 [] userState IxSet.empty 0

showFSItems :: Context u -> IO ()
showFSItems = putStrLn . groom . cFSItems

-- *Main Protolude Data.Tree Text.Groom > putStrLn . groom . V.toList . V.indexed . treeTofsitemsvector $ sampleTree
treeToFSItems
  :: Tree ((RawFilePath -> FSItemId -> FSItem (Context u)), RawFilePath)
  -> IxSet FSItemIxs (FSItem (Context u))
treeToFSItems tree =
  (IxSet.fromList .
   zipWith
     (\(f, name) i -> f name (FSItemId i))
     (flatten (absolutePath "" tree)))
    [0 ..]

-- for testing absolutePath
-- testtree :: Tree (ByteString, RawFilePath)
-- testtree =
--     Node ("directory", "/") [
--       Node ("ctlFile", "ctl" ) []
--     , Node ("inFile", "in" ) []
--     , Node ("readOnlyFile", "out" ) []
--     , Node ("readOnlyFile", "echo" ) []
--     ]
-- below version uses foldTree, call it using
-- > foldTree (absolutePath "") testtree
-- absolutePath :: RawFilePath -> (t,RawFilePath) -> [Tree (t,RawFilePath)] -> Tree (t,RawFilePath)
-- absolutePath parent (f,name) xs =
--   let builtName = combine parent name
--   in Node (f,builtName) (fmap (foldTree (absolutePath builtName)) xs)
-- this is easier to understand than the foldTree version
absolutePath :: RawFilePath -> Tree (t, RawFilePath) -> Tree (t, RawFilePath)
absolutePath parent (Node (f, name) xs) =
  let builtName = fsItemAbsoluteName (combine parent name)
  in Node (f, builtName) (fmap (absolutePath builtName) xs)

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

mkAbsolutePath :: RawFilePath -> AbsolutePath
mkAbsolutePath = AbsolutePath . fsItemAbsoluteName

addFSItem1 :: FSItem (Context u) -> Context u -> Context u
addFSItem1 fsItem c =
  c {cFSItems = updateIx (fsItemId fsItem) fsItem (cFSItems c)}

addFSItem :: (FSItemId -> FSItem (Context u)) -> Context u -> Context u
addFSItem f c =
  let vacantItemId =
        case (IxSet.getOne (cFSItems c @= Vacant)) of
          Nothing ->
            maybe
              ((FSItemId . IxSet.size . cFSItems) c)
              ((\(FSItemId i) -> FSItemId (i + 1)) . fsItemId)
              ((headMay . IxSet.toDescList FSItemId . cFSItems) c)
          Just item -> fsItemId item
      fsItem = f vacantItemId
  in c {cFSItems = updateIx (fsItemId fsItem) fsItem (cFSItems c)}

deleteFSItem1 :: FSItem (Context u) -> Context u -> Context u
deleteFSItem1 me c =
  c {cFSItems = updateIx (fsItemId me) (me {fOccupied = Vacant}) (cFSItems c)}

deleteFSItem :: AbsolutePath -> Context u -> Context u
deleteFSItem name c =
  case IxSet.getOne (cFSItems c @= name) of
    Nothing -> c
    Just me ->
      case IxSet.getOne (cFSItemFids c @= fsItemId me) of
        Nothing ->
          c
          { cFSItems =
              updateIx (fsItemId me) (me {fOccupied = Vacant}) (cFSItems c)
          }
        Just _ -> c
