{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Tests
  ( tests
  , testContext
  ) where

import           Data.Bits
import qualified Data.ByteString     as BS
import           Data.Default
import qualified Data.HashMap.Strict as HashMap
import           Data.Serialize
import qualified Data.Vector         as V
import           Protolude           hiding (get, put)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Network.Simple.TCP

import BitMask

import           Data.NineP
import           Data.NineP.Qid  hiding (Directory)
import qualified Data.NineP.Qid  as Qid
import           Data.NineP.Stat hiding (AppendOnly, Directory)
import qualified Data.NineP.Stat as Stat

--
import Network.NineP
import Network.NineP.Context
import Network.NineP.Directory
import Network.NineP.Error
import Network.NineP.Functions
import Network.NineP.ReadOnlyFile
import Network.NineP.Response
import Network.NineP.Server
import Network.NineP.WriteOnlyFile

-- got this idea from
--  https://jaspervdj.be/posts/2015-03-13-practical-testing-in-haskell.html
tests :: Socket -> TestTree
tests socket =
  testGroup
    "Server"
    [ testCase "testVersion01" (testVersion01 socket)
--     , testCase "testVersion02" ( testVersion02 socket)
--     , testCase "testVersion03" (testVersion03 socket)
--     , testCase "testVersion04" (testVersion04 socket)
--     , testCase "testAttach01" (testAttach01 socket)
--     , testCase "testStat01" (testStat01 socket)
--     , testCase "testStatWriteOnlyFile" (testStatWriteOnlyFile socket)
--     , testCase "testStatReadOnlyFile" (testStatReadOnlyFile socket)
--     , testCase "testClunk01" (testClunk01 socket)
--     , testCase "testWalk01" (testWalk01 socket)
--     , testCase "testWalk02" (testWalk02 socket)
--     , testCase "testWalk03" (testWalk03 socket)
--     , testCase "testWalk04" (testWalk04 socket)
--     , testCase "testWalk05" (testWalk05 socket)
--     , testCase "testOpenWrite" (testOpenWrite socket)
--     , testCase "testOpenRead" (testOpenRead socket)
--     , testCase "testClunk02" (testClunk02 socket)
--     , testCase "testReadDirectoryDir1" (testReadDirectoryDir1 socket)
--     , testCase "testReadDirectoryRoot" (testReadDirectoryRoot socket)
--     , testCase "testWrite01" (testWrite01 socket)
    ]

testContext :: Context
testContext = def {cFSItems = testFSItemsList}

testFSItemsList :: V.Vector (FSItem Context)
testFSItemsList =
  V.fromList
    [ directory "/" 0
    , writeOnlyFile "/in" 1
    , readOnlyFile "/out" 2
    , directory "/dir1" 3
    , writeOnlyFile "/dir1/in" 4
    , readOnlyFile "/dir1/out" 5
    ]

testVersion01 :: Socket -> Assertion
testVersion01 socket = do
  send socket (toNinePFormat (Tversion 8192 Ver9P2000) 1)
  maybeResult <- recv socket 8192
  maybeResult @?= (Just (toNinePFormat (Rversion 8192 Ver9P2000) 1))

-- testVersion02 :: Socket -> Assertion
-- testVersion02 socket =
--   let result = version (Tversion 8192 VerUnknown) testContext
--   in (fst result) @?= (Right (Rversion 8192 VerUnknown))

-- testVersion03 :: Socket -> Assertion
-- testVersion03 socket =
--   let result = version (Tversion 9000 Ver9P2000) testContext
--   in (fst result) @?= (Right (Rversion 9000 Ver9P2000))

-- testVersion04 :: Socket -> Assertion
-- testVersion04 socket =
--   let result = version (Tversion 8000 Ver9P2000) testContext
--   in (fst result) @?= (Right (Rversion 8192 Ver9P2000))

-- testAttach01 :: Socket -> Assertion
-- testAttach01 socket =
--   let result = attach (Tattach 0 0xffffffff "root" "") testContext
--   in (fst result) @?= (Right (Rattach (Qid [Qid.Directory] 0 0)))

-- testStat01 :: Socket -> Assertion
-- testStat01 socket =
--   let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
--       result = stat (Tstat 0) (snd attachresult)
--   in fst result @?=
--      Right
--        (Rstat
--         { rsStat socket =
--             Stat
--             { stTyp = 0
--             , stDev = 0
--             , stQid = Qid {qType = [Qid.Directory], qversion = 0, qPath = 0}
--             , stMode socket =
--                 [ OtherExecutePermission
--                 , OtherWritePermission
--                 , OtherReadPermission
--                 , GroupExecutePermission
--                 , GroupWritePermission
--                 , GroupReadPermission
--                 , UserExecutePermission
--                 , UserWritePermission
--                 , UserReadPermission
--                 , Stat.Directory
--                 ]
--             , stAtime = 0
--             , stMtime = 0
--             , stLength = 0
--             , stName = "/"
--             , stUid = "root"
--             , stGid = "root"
--             , stMuid = "root"
--             }
--         })

-- testClunk01 :: Socket -> Assertion
-- testClunk01 socket =
--   let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
--       statresult = stat (Tstat 0) (snd attachresult)
--       result = clunk (Tclunk 0) (snd statresult)
--   in fst result @?= Right Rclunk

-- testWalk01 :: Socket -> Assertion
-- testWalk01 socket =
--   let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
--       statresult = stat (Tstat 0) (snd attachresult)
--       result = walk (Twalk 0 1 []) (snd statresult)
--   in fst result @?= Right (Rwalk [])

-- testWalk02 :: Socket -> Assertion
-- testWalk02 socket =
--   let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
--       statresult = stat (Tstat 0) (snd attachresult)
--       result = walk (Twalk 0 1 ["/"]) (snd statresult)
--   in fst result @?= Right (Rwalk [])

-- testWalk03 :: Socket -> Assertion
-- testWalk03 socket =
--   let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
--       statresult = stat (Tstat 0) (snd attachresult)
--       result = walk (Twalk 0 1 ["dir1"]) (snd statresult)
--   in fst result @?= Right (Rwalk [Qid [Qid.Directory] 0 3])

-- testWalk04 :: Socket -> Assertion
-- testWalk04 socket =
--   let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
--       statresult = stat (Tstat 0) (snd attachresult)
--       result = walk (Twalk 0 1 ["dir1", "in"]) (snd statresult)
--   in fst result @?=
--      Right (Rwalk [Qid [Qid.Directory] 0 3, Qid [AppendOnly] 0 4])

-- testWalk05 :: Socket -> Assertion
-- testWalk05 socket =
--   let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
--       statresult = stat (Tstat 0) (snd attachresult)
--       walkresult = walk (Twalk 0 1 ["dir1"]) (snd statresult)
--       result = walk (Twalk 1 2 ["in"]) (snd walkresult)
--   in fst result @?= Right (Rwalk [Qid [AppendOnly] 0 4])

-- testOpenWrite :: Socket -> Assertion
-- testOpenWrite = do
--   let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
--       statresult = stat (Tstat 0) (snd attachresult)
--       walkresult = walk (Twalk 0 1 ["in"]) (snd statresult)
--   result <- open (Topen 1 Write) (snd walkresult)
--   fst result @?= Right (Ropen (Qid [AppendOnly] 0 1) 8169)

-- testOpenRead :: Socket -> Assertion
-- testOpenRead = do
--   let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
--       statresult = stat (Tstat 0) (snd attachresult)
--       walkresult = walk (Twalk 0 1 ["out"]) (snd statresult)
--   result <- open (Topen 1 Read) (snd walkresult)
--   fst result @?= Right (Ropen (Qid [] 0 2) 8169)

-- testClunk02 :: Socket -> Assertion
-- testClunk02 = do
--   let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
--       statresult = stat (Tstat 0) (snd attachresult)
--       walkresult = walk (Twalk 0 1 ["in"]) (snd statresult)
--   openresult <- open (Topen 1 Write) (snd walkresult)
--   fst openresult @?= Right (Ropen (Qid [AppendOnly] 0 1) 8169)
--   (HashMap.toList . cFids . snd) openresult @?=
--     [(0, FidState Nothing 0), (1, FidState Nothing 1)]
--   let result = clunk (Tclunk 1) (snd openresult)
--   fst result @?= Right Rclunk
--   (HashMap.toList . cFids . snd) result @?= [(0, FidState Nothing 0)]

-- testReadDirectoryDir1 :: Socket -> Assertion
-- testReadDirectoryDir1 = do
--   let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
--       statresult = stat (Tstat 0) (snd attachresult)
--       walkresult = walk (Twalk 0 1 ["dir1"]) (snd statresult)
--   openresult <- open (Topen 1 Read) (snd walkresult)
--   fst openresult @?= Right (Ropen (Qid [Qid.Directory] 0 3) 8169)
--   (HashMap.toList . cFids . snd) openresult @?=
--     [(0, FidState Nothing 0), (1, FidState Nothing 3)]
--   result <- read (Tread 1 0 8168) (snd openresult)
--   let statBS = runPut . put . dStat . fDetails
--   result @?=
--     Right
--       ((Rread . BS.concat . fmap statBS)
--          [writeOnlyFile "/dir1/in" 4, readOnlyFile "/dir1/out" 5])

-- --       walkresult = walk (Twalk 0 1 []) (snd statresult)
-- --   result @?= Right ((Rread . runPut . put . dStat . fDetails) ( (cFSItems testContexts) V.! 4))
-- testReadDirectoryRoot :: Socket -> Assertion
-- testReadDirectoryRoot = do
--   let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
--       statresult = stat (Tstat 0) (snd attachresult)
--       walkresult = walk (Twalk 0 1 []) (snd statresult)
--   openresult <- open (Topen 1 Read) (snd walkresult)
--   fst openresult @?= Right (Ropen (Qid [Qid.Directory] 0 0) 8169)
--   (HashMap.toList . cFids . snd) openresult @?=
--     [(0, FidState Nothing 0), (1, FidState Nothing 0)]
--   result <- read (Tread 1 0 8168) (snd openresult)
--   --   result @?= Right ((Rread . runPut . put . dStat . fDetails) ( (cFSItems testContexts) V.! 4))
--   let statBS = runPut . put . dStat . fDetails
--   result @?=
--     Right
--       ((Rread . BS.concat . fmap statBS)
--          [writeOnlyFile "/in" 1, readOnlyFile "/out" 2, directory "/dir1" 3])

-- testStatWriteOnlyFile :: Socket -> Assertion
-- testStatWriteOnlyFile socket =
--   let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
--       statresult = stat (Tstat 0) (snd attachresult)
--       walkresult = walk (Twalk 0 1 ["in"]) (snd statresult)
--       result = stat (Tstat 1) (snd walkresult)
--   in fst result @?=
--      Right
--        (Rstat
--         { rsStat socket =
--             Stat
--             { stTyp = 0
--             , stDev = 0
--             , stQid = Qid {qType = [AppendOnly], qversion = 0, qPath = 1}
--             , stMode socket =
--                 [ OtherWritePermission
--                 , GroupWritePermission
--                 , UserWritePermission
--                 , Stat.AppendOnly
--                 ]
--             , stAtime = 0
--             , stMtime = 0
--             , stLength = 0
--             , stName = "in"
--             , stUid = "root"
--             , stGid = "root"
--             , stMuid = "root"
--             }
--         })

-- testStatReadOnlyFile :: Socket -> Assertion
-- testStatReadOnlyFile socket =
--   let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
--       statresult = stat (Tstat 0) (snd attachresult)
--       walkresult = walk (Twalk 0 1 ["out"]) (snd statresult)
--       result = stat (Tstat 1) (snd walkresult)
--   in fst result @?=
--      Right
--        (Rstat
--         { rsStat =
--             Stat
--             { stTyp = 0
--             , stDev = 0
--             , stQid = Qid {qType = [], qversion = 0, qPath = 2}
--             , stMode socket =
--                 [OtherReadPermission, GroupReadPermission, UserReadPermission]
--             , stAtime = 0
--             , stMtime = 0
--             , stLength = 0
--             , stName = "out"
--             , stUid = "root"
--             , stGid = "root"
--             , stMuid = "root"
--             }
--         })

-- testWrite01 :: Socket -> Assertion
-- testWrite01 = do
--   let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
--       statresult = stat (Tstat 0) (snd attachresult)
--       outwalkresult = walk (Twalk 0 1 ["out"]) (snd statresult)
--   outopenresult <- open (Topen 1 Read) (snd outwalkresult)
--   fst outopenresult @?= Right (Ropen (Qid [] 0 2) 8169)
--   let inwalkresult = walk (Twalk 0 2 ["in"]) (snd outopenresult)
--   inopenresult <- open (Topen 2 Write) (snd inwalkresult)
--   fst inopenresult @?= Right (Ropen (Qid [AppendOnly] 0 1) 8169)
--   let writeContents = "testing write" :: Socket -> BS.ByteString
--   writeresult <- write (Twrite 2 0 writeContents) (snd inopenresult)
--   readresult <-
--     read (Tread 1 0 (fromIntegral (BS.length writeContents))) (snd writeresult)
--   Rread writeContents @?= either (\_ -> Rread BS.empty) identity readresult
