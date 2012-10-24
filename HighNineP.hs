{-# LANGUAGE OverloadedStrings, CPP #-}
{-# OPTIONS_GHC -pgmP cpp #-}

module HighNineP 
	( NineFile(..)
	, Config(..)
	, run9PServer
	, boringFile
	, boringDir
	) where

import Control.Concurrent
--import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Control.Monad.Loops
--import qualified Control.Monad.State.Strict as S
import Control.Monad.RWS (evalRWST)
import Control.Monad.Reader.Class (asks)
import qualified Control.Monad.State.Class as S
import Control.Monad.Trans
import qualified Control.Monad.Writer.Class as W
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Map (Map)
import qualified Data.Map as M
import Data.NineP
import Data.Word
import Network.Socket hiding (send, sendTo, recv, recvFrom)
--import Network.Socket.ByteString
import System.IO

import Error

import Debug.Trace

data NineFile =
	RegularFile {
        	read :: Word64 -> Word32 -> IO (B.ByteString),
        	write :: Word64 -> B.ByteString -> IO (Word32),
		remove :: IO (),
		stat :: IO Stat,
		wstat :: Stat -> IO (),
		version :: IO Word32
	} | Directory {
		getFiles :: IO (Map String NineFile),
		remove :: IO (),
		stat :: IO Stat,
		wstat :: Stat -> IO (),
		version :: IO Word32
	}

data Config = Config {
		root :: NineFile
	}

boringStat :: Stat
boringStat = Stat 0 0 (Qid 0 0 0) 0 0 0 0 "boring" "" "" ""

boringFile :: String -> NineFile
boringFile name = RegularFile
        (\_ c -> return $ B.take (fromIntegral c) "i am so very boring")
        (\_ _ -> return 0)
        (return ())
        (return $ boringStat {st_name = name})
        (const $ return ())
	(return 0)

boringDir :: String -> [(String, NineFile)] -> NineFile
boringDir name contents = Directory
	(return $ M.fromList contents)
        (return ())
        (return $ boringStat {st_name = name})
        (const $ return ())
	(return 0)

run9PServer :: Config -> IO ()
run9PServer cfg = do
	s <- socket AF_INET Stream defaultProtocol
	setSocketOption s ReuseAddr 1
	bindSocket s (SockAddrInet 4242 iNADDR_ANY)
	listen s 10
	serve s cfg

serve :: Socket -> Config -> IO ()
serve s cfg = forever $ accept s >>= (
		\(s, _) -> (doClient cfg) =<< (liftIO $ socketToHandle s ReadWriteMode))

doClient :: Config -> Handle -> IO ()
doClient cfg h = do
	putStrLn "yay, a client"
	hSetBuffering h NoBuffering
	chan <- (newChan :: IO (Chan Msg))
	st <- forkIO $ sender (readChan chan) (B.hPut h)
	receiver cfg h (writeChan chan)
	putStrLn "bye!"
	killThread st
	hClose h

recvPacket :: Handle -> IO Msg
recvPacket h = do
	-- TODO error reporting
	putStrLn "got packet"
	s <- B.hGet h 4
	let l = fromIntegral $ runGet getWord32le $ assert (B.length s == 4) s
	p <- B.hGet h $ l - 4
	let m = runGet (get :: Get Msg) (B.append s p)
	print m
	return m

sender :: IO Msg -> (ByteString -> IO ()) -> IO ()
sender get say = forever $ do
	(say . runPut . put . join traceShow) =<< get

receiver :: Config -> Handle -> (Msg -> IO ()) -> IO ()
receiver cfg h say = evalRWST (iterateUntil id (do
		W.tell () -- satisfy the typechecker
		p <- liftIO $ recvPacket h
		let Msg typ t m = p
		case typ of
		{-
			TTversion -> (liftIO . say . Msg TRversion t) =<< rversion (Rversion) m
			TTattach -> (liftIO . say . Msg TRattach t) =<< rattach (Rattach) m
			TTwalk -> (liftIO . say . Msg TRwalk t) =<< rwalk (Rwalk) m
			-}
			TTflush -> return ()	-- not implemented
#define MSG(x) TT##x -> (liftIO . say . Msg TR##x t) =<< r##x (R##x) m
			MSG(version)
			MSG(attach)
			--MSG(walk)
--			MSG(stat)
			MSG(clunk)
#undef MSG
		return False) >> return ()
	) cfg (M.empty :: Map Word32 NineFile) >> return ()

makeQid :: NineFile -> Qid
makeQid = const $ Qid 0 0 0

--TODO version check
rversion c (Tversion s _) = return $ c s "9P2000"

{-
rattach :: (Qid -> VarMsg) -> VarMsg -> S.StateT (Map Word32 Word64) IO VarMsg
-}
rattach c (Tattach fid _ _ _) = do
	--q <- asks rootQIDP
	--mf <- asks mkFile
	--f <- lift $ mf q
	root <- asks root
	S.modify (M.insert fid root)
	--return $ c $ Qid (typ f) 0 q
	return $ c $ makeQid root

--walk :: [String] -> NineFile -> NineFile
walk :: [String] -> NineFile -> ErrorT NineError IO NineFile
walk [] f = return f
--walk (x:[]) (RegularFile _ _ _ _ _ _) = return f
walk (x:xs) (RegularFile _ _ _ _ _ _) = throwError ENotADir
walk (x:xs) (Directory gF _ _ _ _) = do
	m <- lift gF
	case M.lookup x m of
		Nothing -> throwError $ ENoFile x
		Just f -> do
			walk xs f

--rwalk c (Twalk fid newfid path) = do
--	let file 
--	S.modify (M.insert newfid file)

rclunk c (Tclunk fid) = do
	S.modify (M.delete fid)
	return $ c
