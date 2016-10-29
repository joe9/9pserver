{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.NineP.Response where

import qualified Data.ByteString                  as BS
import qualified Data.HashMap.Strict              as HashMap
import           Data.Maybe
import           Data.String.Conversions
import qualified Data.Vector                      as V
import           GHC.Show
import           Protolude                        hiding (show)
import           System.Posix.ByteString.FilePath
import           System.Posix.FilePath

import Data.NineP
import Data.NineP.Stat
import Network.NineP.Context
import Network.NineP.Error
import Network.NineP.Functions

-- TODO Not bothering with T.chunksOf on size.
-- assuming that the length of bytestring will be the same as that of
-- Text for calculating msize using T.length
version :: Tversion -> (Context u) -> (Either Rerror Rversion, (Context u))
version (Tversion s tversion) context =
  let newSize = max (fromIntegral s) (cMaxMessageSize context)
      updatedContext =
        if tversion == VerUnknown
          then context {cMaxMessageSize = newSize}
          else (resetContext context) {cMaxMessageSize = newSize}
  in ((Right . Rversion (fromIntegral newSize)) tversion, updatedContext)

-- below for debugging
-- version :: Tversion -> (Context u) -> (Either Rerror Rversion, (Context u))
-- version (Tversion s rversion) context =
--   ( (Right . Rversion (fromIntegral 8192)) "9P2000"
--      , context)
-- checkPerms :: (Monad m, EmbedIO m) => NineFile m -> Word8 -> Nine m ()
-- checkPerms f want = do
--     s <- getStat f
--     checkPerms' (st_mode s) want
-- checkPerms' :: (Monad m, EmbedIO m) => Word32 -> Word8 -> Nine m ()
-- checkPerms' have want = do
--     -- TODO stop presuming we are owners
--     let checkRead = unless (testBit have 2) $ throw EPermissionDenied
--     let checkWrite = unless (testBit have 1) $ throw EPermissionDenied
--     let checkExec = unless (testBit have 0) $ throw EPermissionDenied
--     when (testBit want 4) $ do
--         checkWrite
--         throw $ ENotImplemented "OTRUNC"
--     when (testBit want 6) $ do
--         throw $ ENotImplemented "ORCLOSE"
--     case want of
--         0 -> checkRead
--         1 -> checkWrite
--         2 -> checkRead >> checkWrite
--         3 -> checkExec
-- getQidTyp :: Stat -> Word8
-- getQidTyp s = fromIntegral $ shift (st_mode s) 24
-- makeQid :: (Monad m, EmbedIO m) => NineFile m -> Nine m Qid
-- makeQid x = do
--     s <- getStat x
--     return $ Qid (getQidTyp s) 0 42
runEitherFunction :: (Either NineError a, t) -> (a -> b) -> (Either Rerror b, t)
runEitherFunction f cf =
  case f of
    (Left e, ulc)  -> (rerror e, ulc)
    (Right v, urc) -> ((Right . cf) v, urc)

runMaybeFunction :: (Maybe NineError, t) -> b -> (Either Rerror b, t)
runMaybeFunction f cf =
  case f of
    (Just e, ulc)  -> (rerror e, ulc)
    (Nothing, urc) -> (Right cf, urc)

rerror :: NineError -> Either Rerror b
rerror = Left . Rerror . showNineError

-- Not checking if afid == (u32int)~0, just not authenticating for all
-- 0 == root directory path == index in cFSItems
-- if fid is already in use, error out
attach :: Tattach -> (Context u) -> (Either Rerror Rattach, (Context u))
attach (Tattach fid afid uname aname) c =
  case HashMap.lookup fid (cFids c) of
    Nothing ->
      if aname == "/" || BS.null aname || fid == 0
        then let f d =
                   runEitherFunction
                     (((dAttach . fDetails) d) fid afid uname aname 0 d c)
                     Rattach
             in maybe
                  ( rerror (OtherError "attach: 0 attach point does not exist")
                  , c)
                  f
                  ((cFSItems c) V.!? 0)
        else (rerror (OtherError "attach: invalid attach point"), c)
    Just _ -> (rerror (OtherError "attach: fid is already in use"), c)

-- qid :: Vector (FSItem (Context u)) -> Int -> Either NineError Qid
-- qid v i =
--   case v V.!? i of
--     Nothing -> Left EInval
--     (Just Free) -> Left EInval
--     -- TODO get the type from stat
--     (Just (Dir d _)) -> Right (Qid [Directory] (dVersion d) (fromIntegral i))
--     -- TODO get the type from stat
--     (Just (File f)) -> Right (Qid [NineP.File] (fVersion f) (fromIntegral i))
-- -- 0 == root directory path == index in cFSItems
-- attach :: Tattach -> (Context u) -> (Either Rerror Rattach, (Context u))
-- attach (Tattach fid _ _ _) c =
--   case qid (cFSItems c) 0 of
--     Left e  -> ((Left . Rerror . showNineError) e, c)
--     Right q -> ((Right . Rattach) q, uc)
--   where
--     uc = c {cFids = HashMap.insert fid 0 (cFids c)}
-- TODO The actual file is not removed on the server unless the fid had been opened with ORCLOSE.
clunk :: Tclunk -> (Context u) -> (Either Rerror Rclunk, (Context u))
clunk (Tclunk fid) c = maybe (rerror EInval, c) f ((cFSItems c) V.!? 0)
  where
    f d = runMaybeFunction (((dClunk . fDetails) d) fid d c) Rclunk

flush :: Tflush -> (Context u) -> (Either Rerror Rflush, (Context u))
flush (Tflush _) c = (Right Rflush, c)

--  TODO This request will fail if the client does not have write permission in the parent directory.
remove :: Tremove -> (Context u) -> (Either Rerror Rremove, (Context u))
remove (Tremove fid) c =
  case HashMap.lookup fid (cFids c) of
    Nothing -> (rerror (ENoFile "fid cannot be found"), c)
    Just fds ->
      maybe (rerror EInval, c) f ((cFSItems c) V.!? (fidFSItemsIndex fds))
      where f d =
              runMaybeFunction (((dRemove . fDetails) d) fid fds d c) Rremove

-- ropen (Msg _ t (Topen fid mode)) = do
--     f <- lookup fid
--     checkPerms f mode
--     qid <- open f
--     iou <- iounit
--     return $ return $ Msg TRopen t $ Ropen qid iou
open :: Topen -> (Context u) -> IO (Either Rerror Ropen, (Context u))
open (Topen fid mode) c =
  case HashMap.lookup fid (cFids c) of
    Nothing -> return (rerror (ENoFile "fid cannot be found"), c)
    Just fds ->
      case (cFSItems c) V.!? (fidFSItemsIndex fds) of
        Nothing -> return (rerror EInval, c)
        Just d -> do
          result <- ((dOpen . fDetails) d) fid mode fds d c
          return (runEitherFunction result (uncurry Ropen))

create :: Tcreate -> (Context u) -> (Either Rerror Rcreate, (Context u))
create (Tcreate fid name permissions mode) c =
  case HashMap.lookup fid (cFids c) of
    Nothing -> (rerror (ENoFile "fid cannot be found"), c)
    Just fds ->
      maybe (rerror EInval, c) f ((cFSItems c) V.!? (fidFSItemsIndex fds))
      where f d =
              runEitherFunction
                (((dCreate . fDetails) d) fid name permissions mode d c)
                (\(a, b) -> Rcreate a b)

-- rread :: (Monad m, EmbedIO m) => Msg -> Nine m [Msg]
-- rread (Msg _ t (Tread fid offset count)) = do
--     f <- lookup fid
--     u <- iounit
--     checkPerms f 0
--     let    splitMsg d s = let r = splitMsg' d s in if null r then [B.empty] else r
--            splitMsg' d s =
--              if B.null d
--              then []
--              else let (a, b) = B.splitAt s d in a : splitMsg' b s
--     case f of
--         RegularFile {} -> do
--             d <- call $ (read f) offset count
--             mapM (return . Msg TRread t . Rread) $ splitMsg d $ fromIntegral u
--         Directory {} -> do
--             contents <- call $ getFiles f
--             s <- mapM getStat $ contents
--             let d = runPut $ mapM_ put s
--             mapM (return . Msg TRread t . Rread) $ splitMsg (B.drop (fromIntegral offset) d) $ fromIntegral u
read :: Tread -> (Context u) -> IO (Either Rerror Rread)
read (Tread fid offset count) c =
  case HashMap.lookup fid (cFids c) of
    Nothing -> return (rerror (ENoFile "fid cannot be found"))
    Just fds ->
      case (cFSItems c) V.!? (fidFSItemsIndex fds) of
        Nothing -> return (rerror EInval)
        Just d -> do
          result <- ((dRead . fDetails) d) fid offset count fds d c
          case result of
            Left e  -> return (rerror e)
            Right v -> return ((Right . Rread) v)

write :: Twrite -> (Context u) -> IO (Either Rerror Rwrite, (Context u))
write (Twrite fid offset dat) c =
  case HashMap.lookup fid (cFids c) of
    Nothing -> return (rerror (ENoFile "fid cannot be found"), c)
    Just fds ->
      case (cFSItems c) V.!? (fidFSItemsIndex fds) of
        Nothing -> return (rerror EInval, c)
        Just d -> do
          result <- ((dWrite . fDetails) d) fid offset dat fds d c
          return (runEitherFunction result Rwrite)

checkBlockedReads :: (Context u) -> IO ([(Tag, Rerror)], (Context u))
checkBlockedReads c = do
  checkeds <- (mapM checkBlockedRead . cBlockedReads) c
  let (blockedReads, responses) = partitionEithers checkeds
  return (catMaybes responses, c {cBlockedReads = blockedReads})

checkBlockedRead :: BlockedRead -> IO (Either BlockedRead (Maybe (Tag, Rerror)))
checkBlockedRead blockedRead = do
  v <- poll (bAsync blockedRead)
  case v
       -- still pending
        of
    Nothing -> return (Left blockedRead)
    -- completed with an exception
    (Just (Left e)) ->
      return
        (Right
           (Just
              ( bTag blockedRead
              , (Rerror . showNineError . OtherError . cs . show) e)))
    -- completed successfully
    (Just (Right _)) -> return (Right Nothing)

stat :: Tstat -> (Context u) -> (Either Rerror Rstat, (Context u))
stat (Tstat fid) c =
  case HashMap.lookup fid (cFids c) of
    Nothing -> (rerror (ENoFile "fid cannot be found"), c)
    Just fds ->
      maybe (rerror EInval, c) f ((cFSItems c) V.!? (fidFSItemsIndex fds))
      where f d = runEitherFunction (((dReadStat . fDetails) d) fid d c) Rstat

wstat :: Twstat -> (Context u) -> (Either Rerror Rwstat, (Context u))
wstat (Twstat fid stat) c =
  case HashMap.lookup fid (cFids c) of
    Nothing -> (rerror (ENoFile "fid cannot be found"), c)
    Just fds ->
      maybe (rerror EInval, c) f ((cFSItems c) V.!? (fidFSItemsIndex fds))
      where f d =
              runMaybeFunction
                (((dWriteStat . fDetails) d) fid stat fds d c)
                Rwstat

-- TODO ../
walk :: Twalk -> (Context u) -> (Either Rerror Rwalk, (Context u))
walk (Twalk fid newfid nwnames) c
  | isJust (HashMap.lookup newfid (cFids c)) && fid /= newfid =
    (rerror (OtherError "walk: proposed newfid is already in use"), c)
  | otherwise =
    case HashMap.lookup fid (cFids c) of
      Nothing -> (rerror (OtherError "walk: invalid fid"), c)
      Just fidState ->
        if null (filterOutJustSlash nwnames)
          then ( Right (Rwalk [])
               , c
                 { cFids =
                     HashMap.insert
                       newfid
                       (FidState Nothing (fidFSItemsIndex fidState))
                       (cFids c)
                 })
          else let f fsItem =
                     runEitherFunction
                       (((dWalk . fDetails) fsItem)
                          newfid
                          ((dAbsoluteName . fDetails) fsItem)
                          []
                          (filterOutJustSlash nwnames)
                          c)
                       Rwalk
               in maybe
                    (rerror (OtherError "walk: invalid FSItemsIndex"), c)
                    f
                    ((cFSItems c) V.!? (fidFSItemsIndex fidState))

filterOutJustSlash :: [RawFilePath] -> [RawFilePath]
filterOutJustSlash = filter ((/=) "/")
-- getStat :: (Monad m, EmbedIO m) => NineFile m -> Nine m Stat
-- getStat f = do
--     let fixDirBit = (case f of
--                 (RegularFile {}) -> flip clearBit 31
--                 (Directory {}) -> flip setBit 31
--             )
--     s <- call $ stat f
--     return s { st_mode = fixDirBit $ st_mode s,
--         st_qid = (st_qid s) { qid_typ = getQidTyp s } }
-- rstat (Msg _ t (Tstat fid)) = do
--     f <- lookup fid
--     case f of
--         RegularFile {} -> do
--             s <- getStat f
--             return $ return $ Msg TRstat t $ Rstat $ [s]
--         Directory {} -> do
--             mys <- getStat f
--             return $ return $ Msg TRstat t $ Rstat $ return $ mys
-- rwstat (Msg _ t (Twstat fid stat)) = do
--     -- TODO check perms
--     f <- lookup fid
--     -- TODO implement
--     return $ return $ Msg TRwstat t $ Rwstat
-- stat :: Tstat -> (Context u) -> (Either Rerror Rstat, (Context u))
-- stat (Tstat fid) c =
--   case HashMap.lookup fid (cFids c) of
--     Nothing -> ( (Left . Rerror . showNineError) (ENoFile "fid cannot be found"), c)
--     Just i -> (Right Rstat
--              , c { cFids = HashMap.delete fid (cFids c)
--                  , cFSItems = V.modify (\v -> DVM.write v i Free) (cFSItems c)
--                  })
-- walk :: (Monad m, EmbedIO m) => [Qid] -> [String] -> NineFile m -> Nine m (NineFile m, [Qid])
-- walk qs [] f = return (f, qs)
-- walk qs (x:xs) (RegularFile {}) = throw ENotADir
-- walk qs (x:xs) d@(Directory {}) = do
--     f <- call $ desc d x
--     q <- makeQid f
--     walk (q:qs) xs f
-- walk' :: (Monad m, EmbedIO m) => [String] -> NineFile m -> Nine m (NineFile m, [Qid])
-- walk' = walk []
-- rwalk (Msg _ t (Twalk fid newfid path)) = do
--     f <- lookup fid
--     (nf, qs) <- walk' path f
--     insert newfid nf
--     return $ return $ Msg TRwalk t $ Rwalk $ qs
-- open :: (Monad m, EmbedIO m) => NineFile m -> Nine m Qid
-- open f = do
--     makeQid $ f
-- ropen (Msg _ t (Topen fid mode)) = do
--     f <- lookup fid
--     checkPerms f mode
--     qid <- open f
--     iou <- iounit
--     return $ return $ Msg TRopen t $ Ropen qid iou
-- rcreate (Msg _ t (Tcreate fid name perm mode)) = do
--     f <- lookup fid
--     -- TODO check permissions to create
--     case f of
--         RegularFile {} -> throw ENotADir
--         Directory {} -> do
--             nf <- call $ (create f) name perm
--             insert fid nf
--             qid <- open f
--             iou <- iounit
--             return $ return $ Msg TRcreate t $ Rcreate qid iou
-- desc :: (Monad m, EmbedIO m) => NineFile m -> String -> m (NineFile m)
-- desc f ".." = do
--     mp <- parent f
--     return $ case mp of
--         Just p -> p
--         Nothing -> f
-- desc f s = descend f s
-- rauth (Msg {}) = do
--     throw ENoAuthRequired
-- rread :: (Monad m, EmbedIO m) => Msg -> Nine m [Msg]
-- rread (Msg _ t (Tread fid offset count)) = do
--     f <- lookup fid
--     u <- iounit
--     checkPerms f 0
--     let    splitMsg d s = let r = splitMsg' d s in if null r then [B.empty] else r
--            splitMsg' d s =
--              if B.null d
--              then []
--              else let (a, b) = B.splitAt s d in a : splitMsg' b s
--     case f of
--         RegularFile {} -> do
--             d <- call $ (read f) offset count
--             mapM (return . Msg TRread t . Rread) $ splitMsg d $ fromIntegral u
--         Directory {} -> do
--             contents <- call $ getFiles f
--             s <- mapM getStat $ contents
--             let d = runPut $ mapM_ put s
--             mapM (return . Msg TRread t . Rread) $ splitMsg (B.drop (fromIntegral offset) d) $ fromIntegral u
-- --rwrite :: Msg -> Nine m [Msg]
-- rwrite (Msg _ t (Twrite fid offset d)) = do
--     f <- lookup fid
--     checkPerms f 1
--     case f of
--         Directory {} -> throw EDir
--         RegularFile {} -> do
--             c <- call $ (write f) offset d
--             return $ return $ Msg TRwrite t $ Rwrite c
