{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Response.Tests
  ( tests
  ) where

import           Control.Concurrent.STM.TQueue
import qualified Data.ByteString               as BS
import qualified Data.HashMap.Strict           as HashMap
import           Data.IxSet.Typed              as IxSet
import           Data.Serialize
import           Data.Tree
import qualified Data.Vector                   as V
import           Protolude                     hiding (get, put)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Groom

import           Data.NineP
import           Data.NineP.OpenMode
import           Data.NineP.Qid      hiding (Directory)
import qualified Data.NineP.Qid      as Qid
import           Data.NineP.Stat     hiding (AppendOnly, Directory)
import qualified Data.NineP.Stat     as Stat

--
import Network.NineP
import Network.NineP.Directory
import Network.NineP.ReadOnlyPipe
import Network.NineP.Response
import Network.NineP.WriteOnlyPipe

-- got this idea from
--  https://jaspervdj.be/posts/2015-03-13-practical-testing-in-haskell.html
tests :: TestTree
tests =
  testGroup
    "Response"
    [ testCase "testVersion01" testVersion01
    , testCase "testVersion02" testVersion02
    , testCase "testVersion03" testVersion03
    , testCase "testVersion04" testVersion04
    , testCase "testAttach01" testAttach01
    , testCase "testStat01" testStat01
    , testCase "testStatWriteOnlyFile" testStatWriteOnlyFile
    , testCase "testStatReadOnlyFile" testStatReadOnlyFile
    , testCase "testClunk01" testClunk01
    , testCase "testWalk01" testWalk01
    , testCase "testWalk02" testWalk02
    , testCase "testWalk03" testWalk03
    , testCase "testWalk04" testWalk04
    , testCase "testWalk05" testWalk05
    , testCase "testOpenWrite" testOpenWrite
    , testCase "testOpenRead" testOpenRead
    , testCase "testClunk02" testClunk02
    , testCase "testReadDirectoryDir1" testReadDirectoryDir1
    , testCase "testReadDirectoryRoot" testReadDirectoryRoot
    , testCase "testWrite01" testWrite01
    ]

testContext :: Context ()
testContext =
  (defaultContext ())
  {cFSItems = testFSItemsList, cFSItemIdCounter = IxSet.size testFSItemsList}

testFSItemsList :: IxSet FSItemIxs (FSItem (Context u))
testFSItemsList =
  treeToFSItems
    (Node
       (directory, "/")
       [ Node (writeOnlyPipe, "in") []
       , Node (readOnlyPipe, "out") []
       , Node
           (directory, "dir1")
           [Node (writeOnlyPipe, "in") [], Node (readOnlyPipe, "out") []]
       ])

testVersion01 :: Assertion
testVersion01 =
  let result = version (Tversion 8192 Ver9P2000) testContext
  in (fst result) @?= (Right (Rversion 8192 Ver9P2000))

testVersion02 :: Assertion
testVersion02 =
  let result = version (Tversion 8192 VerUnknown) testContext
  in (fst result) @?= (Right (Rversion 8192 VerUnknown))

testVersion03 :: Assertion
testVersion03 =
  let result = version (Tversion 9000 Ver9P2000) testContext
  in (fst result) @?= (Right (Rversion 9000 Ver9P2000))

testVersion04 :: Assertion
testVersion04 =
  let result = version (Tversion 8000 Ver9P2000) testContext
  in (fst result) @?= (Right (Rversion 8192 Ver9P2000))

testAttach01 :: Assertion
testAttach01 =
  let result = attach (Tattach 0 0xffffffff "root" "") testContext
  in (fst result) @?= (Right (Rattach (Qid [Qid.Directory] 0 0)))

testStat01 :: Assertion
testStat01 =
  let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
      result = stat (Tstat 0) (snd attachresult)
  in fst result @?=
     Right
       (Rstat
        { rsStat =
            Stat
            { stTyp = 0
            , stDev = 0
            , stQid = Qid {qType = [Qid.Directory], qversion = 0, qPath = 0}
            , stMode =
                [ OtherExecutePermission
                , OtherWritePermission
                , OtherReadPermission
                , GroupExecutePermission
                , GroupWritePermission
                , GroupReadPermission
                , UserExecutePermission
                , UserWritePermission
                , UserReadPermission
                , Stat.Directory
                ]
            , stAtime = 0
            , stMtime = 0
            , stLength = 0
            , stName = "/"
            , stUid = "root"
            , stGid = "root"
            , stMuid = "root"
            }
        })

testClunk01 :: Assertion
testClunk01 = do
  let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
      statresult = stat (Tstat 0) (snd attachresult)
  result <- clunk (Tclunk 0) (snd statresult)
  fst result @?= Right Rclunk

testWalk01 :: Assertion
testWalk01 =
  let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
      statresult = stat (Tstat 0) (snd attachresult)
      result = walk (Twalk 0 1 []) (snd statresult)
  in fst result @?= Right (Rwalk [])

testWalk02 :: Assertion
testWalk02 =
  let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
      statresult = stat (Tstat 0) (snd attachresult)
      result = walk (Twalk 0 1 ["/"]) (snd statresult)
  in fst result @?= Right (Rwalk [])

testWalk03 :: Assertion
testWalk03 = do
  let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
  (fst attachresult) @?= (Right (Rattach (Qid [Qid.Directory] 0 0)))
  let statresult = stat (Tstat 0) (snd attachresult)
      result = walk (Twalk 0 1 ["dir1"]) (snd statresult)
  fst result @?= Right (Rwalk [Qid [Qid.Directory] 0 3])

--   (putStrLn . groom . cFSItems . snd) statresult
testWalk04 :: Assertion
testWalk04 =
  let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
      statresult = stat (Tstat 0) (snd attachresult)
      result = walk (Twalk 0 1 ["dir1", "in"]) (snd statresult)
  in fst result @?=
     Right (Rwalk [Qid [Qid.Directory] 0 3, Qid [AppendOnly] 0 4])

testWalk05 :: Assertion
testWalk05 =
  let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
      statresult = stat (Tstat 0) (snd attachresult)
      walkresult = walk (Twalk 0 1 ["dir1"]) (snd statresult)
      result = walk (Twalk 1 2 ["in"]) (snd walkresult)
  in fst result @?= Right (Rwalk [Qid [AppendOnly] 0 4])

testOpenWrite :: Assertion
testOpenWrite = do
  let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
      statresult = stat (Tstat 0) (snd attachresult)
      walkresult = walk (Twalk 0 1 ["in"]) (snd statresult)
  result <- open (Topen 1 Write) (snd walkresult)
  fst result @?= Right (Ropen (Qid [AppendOnly] 0 1) 8169)

testOpenRead :: Assertion
testOpenRead = do
  let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
      statresult = stat (Tstat 0) (snd attachresult)
      walkresult = walk (Twalk 0 1 ["out"]) (snd statresult)
  result <- open (Topen 1 Read) (snd walkresult)
  fst result @?= Right (Ropen (Qid [] 0 2) 8169)

testClunk02 :: Assertion
testClunk02 = do
  let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
      statresult = stat (Tstat 0) (snd attachresult)
      walkresult = walk (Twalk 0 1 ["in"]) (snd statresult)
  openresult <- open (Topen 1 Write) (snd walkresult)
  fst openresult @?= Right (Ropen (Qid [AppendOnly] 0 1) 8169)
  (HashMap.toList . cFids . snd) openresult @?=
    [ (0, FidState Nothing Nothing (FidId 0))
    , (1, FidState Nothing Nothing (FidId 1))
    ]
  result <- clunk (Tclunk 1) (snd openresult)
  fst result @?= Right Rclunk
  (HashMap.toList . cFids . snd) result @?=
    [(0, FidState Nothing Nothing (FidId 0))]

testReadDirectoryDir1 :: Assertion
testReadDirectoryDir1 = do
  let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
      statresult = stat (Tstat 0) (snd attachresult)
      walkresult = walk (Twalk 0 1 ["dir1"]) (snd statresult)
  openresult <- open (Topen 1 Read) (snd walkresult)
  fst openresult @?= Right (Ropen (Qid [Qid.Directory] 0 3) 8169)
  (HashMap.toList . cFids . snd) openresult @?=
    [ (0, FidState Nothing Nothing (FidId 0))
    , (1, FidState Nothing Nothing (FidId 1))
    ]
  (IxSet.toList . cFSItemFids . snd) openresult @?=
    [(FSItemFid (FSItemId 0) (FidId 0)), (FSItemFid (FSItemId 3) (FidId 1))]
  result <- read (Tread 1 0 8168) (snd openresult)
  let statBS = runPut . put . dStat . fDetails
  fst result @?=
    ((ReadResponse . BS.concat . fmap statBS)
       [ writeOnlyPipe "/dir1/in" (FSItemId 4)
       , readOnlyPipe "/dir1/out" (FSItemId 5)
       ])

--       walkresult = walk (Twalk 0 1 []) (snd statresult)
--   result @?= Right ((Rread . runPut . put . dStat . fDetails) ( (cFSItems testContexts) V.! 4))
testReadDirectoryRoot :: Assertion
testReadDirectoryRoot = do
  let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
      statresult = stat (Tstat 0) (snd attachresult)
      walkresult = walk (Twalk 0 1 []) (snd statresult)
  openresult <- open (Topen 1 Read) (snd walkresult)
  fst openresult @?= Right (Ropen (Qid [Qid.Directory] 0 0) 8169)
  (HashMap.toList . cFids . snd) openresult @?=
    [ (0, FidState Nothing Nothing (FidId 0))
    , (1, FidState Nothing Nothing (FidId 1))
    ]
  result <- read (Tread 1 0 8168) (snd openresult)
  --   result @?= Right ((Rread . runPut . put . dStat . fDetails) ( (cFSItems testContexts) V.! 4))
  let statBS = runPut . put . dStat . fDetails
  fst result @?=
    ((ReadResponse . BS.concat . fmap statBS)
       [ writeOnlyPipe "/in" (FSItemId 1)
       , readOnlyPipe "/out" (FSItemId 2)
       , directory "/dir1" (FSItemId 3)
       ])

testStatWriteOnlyFile :: Assertion
testStatWriteOnlyFile =
  let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
      statresult = stat (Tstat 0) (snd attachresult)
      walkresult = walk (Twalk 0 1 ["in"]) (snd statresult)
      result = stat (Tstat 1) (snd walkresult)
  in fst result @?=
     Right
       (Rstat
        { rsStat =
            Stat
            { stTyp = 0
            , stDev = 0
            , stQid = Qid {qType = [AppendOnly], qversion = 0, qPath = 1}
            , stMode =
                [ OtherWritePermission
                , GroupWritePermission
                , UserWritePermission
                , Stat.AppendOnly
                ]
            , stAtime = 0
            , stMtime = 0
            , stLength = 0
            , stName = "in"
            , stUid = "root"
            , stGid = "root"
            , stMuid = "root"
            }
        })

testStatReadOnlyFile :: Assertion
testStatReadOnlyFile =
  let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
      statresult = stat (Tstat 0) (snd attachresult)
      walkresult = walk (Twalk 0 1 ["out"]) (snd statresult)
      result = stat (Tstat 1) (snd walkresult)
  in fst result @?=
     Right
       (Rstat
        { rsStat =
            Stat
            { stTyp = 0
            , stDev = 0
            , stQid = Qid {qType = [], qversion = 0, qPath = 2}
            , stMode =
                [OtherReadPermission, GroupReadPermission, UserReadPermission]
            , stAtime = 0
            , stMtime = 0
            , stLength = 0
            , stName = "out"
            , stUid = "root"
            , stGid = "root"
            , stMuid = "root"
            }
        })

testWrite01 :: Assertion
testWrite01 = do
  let attachresult = attach (Tattach 0 0xffffffff "root" "") testContext
      statresult = stat (Tstat 0) (snd attachresult)
      outwalkresult = walk (Twalk 0 1 ["out"]) (snd statresult)
  outopenresult <- open (Topen 1 Read) (snd outwalkresult)
  fst outopenresult @?= Right (Ropen (Qid [] 0 2) 8169)
  let inwalkresult = walk (Twalk 0 2 ["in"]) (snd outopenresult)
  inopenresult <- open (Topen 2 Write) (snd inwalkresult)
  fst inopenresult @?= Right (Ropen (Qid [AppendOnly] 0 1) 8169)
  let writeContents = "testing write" :: BS.ByteString
  writeresult <- write (Twrite 2 0 writeContents) (snd inopenresult)
  (ReadQ readQ count (FidId 1), _) <-
    read (Tread 1 0 (fromIntegral (BS.length writeContents))) (snd writeresult)
  (fromIntegral (BS.length writeContents)) @?= count
  contents <- atomically (readTQueue readQ)
  writeContents @?= contents
