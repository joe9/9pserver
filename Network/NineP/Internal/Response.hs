{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.NineP.Internal.Response where

import qualified Data.ByteString     as BS
import           Data.HashMap.Strict as HashMap
import           Data.Maybe
import qualified Data.Text           as T
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import qualified  Data.Vector.Mutable as DVM
import           Data.Word
import           Protolude
import           TextShow

import           Data.NineP                     hiding (File)
import qualified Data.NineP                     as NineP
import           Network.NineP.Error
import           Network.NineP.Internal.Context
import           Network.NineP.Internal.File

-- TODO Not bothering with T.chunksOf on size.
-- assuming that the length of bytestring will be the same as that of
-- Text for calculating msize using T.length
version :: Tversion -> Context -> (Either Rerror Rversion, Context)
version (Tversion s rversion) context =
  let protoVersion = validateNineVersion rversion
      newSize =
        if fromIntegral s > cMaxMessageSize updatedContext
          then fromIntegral s
          else cMaxMessageSize updatedContext
      updatedContext =
        if protoVersion == VerUnknown
          then context {cMaxMessageSize = newSize}
          else (resetContext context) {cMaxMessageSize = newSize}
  in ( (Right . Rversion (fromIntegral newSize)) (showNineVersion protoVersion)
     , updatedContext)

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
qid :: Vector (FSItem Context) -> Int -> Either NineError Qid
qid v i =
  case v V.!? i of
    Nothing -> Left EInval
    (Just Free) -> Left EInval
    -- TODO get the type from stat
    (Just (Dir d _)) -> Right (Qid [Directory] (dVersion d) (fromIntegral i))
    -- TODO get the type from stat
    (Just (File f)) -> Right (Qid [NineP.File] (fVersion f) (fromIntegral i))

-- 0 == root directory path == index in cQids
attach :: Tattach -> Context -> (Either Rerror Rattach, Context)
attach (Tattach fid _ _ _) c =
  case qid (cQids c) 0 of
    Left e  -> ((Left . Rerror . showNineError) e, c)
    Right q -> ((Right . Rattach) q, uc)
  where
    uc = c {cFids = HashMap.insert fid 0 (cFids c)}

-- TODO The actual file is not removed on the server unless the fid had been opened with ORCLOSE.
clunk :: Tclunk -> Context -> (Either Rerror Rclunk, Context)
clunk (Tclunk fid) c =
  (Right Rclunk, c {cFids = HashMap.delete fid (cFids c)})

flush :: Tflush -> Context -> (Either Rerror Rflush, Context)
flush (Tflush _) c = (Right Rflush, c)

--  TODO This request will fail if the client does not have write permission in the parent directory.
remove :: Tremove -> Context -> (Either Rerror Rremove, Context)
remove (Tremove fid) c =
  case HashMap.lookup fid (cFids c) of
    Nothing -> ( (Left . Rerror . showNineError) (ENoFile "fid cannot be found"), c)
    Just i -> (Right Rremove
             , c { cFids = HashMap.delete fid (cFids c)
                 , cQids = V.modify (\v -> DVM.write v i Free) (cQids c)
                 })

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

stat :: Tstat -> Context -> (Either Rerror Rstat, Context)
stat (Tstat fid) c =
  case HashMap.lookup fid (cFids c) of
    Nothing -> ( (Left . Rerror . showNineError) (ENoFile "fid cannot be found"), c)
    Just i -> (Right Rstat
             , c { cFids = HashMap.delete fid (cFids c)
                 , cQids = V.modify (\v -> DVM.write v i Free) (cQids c)
                 })

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
