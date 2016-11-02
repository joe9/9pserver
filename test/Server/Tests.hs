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
import           Network.Simple.TCP
import           Protolude           hiding (get, put)
import           Test.Tasty
import           Test.Tasty.HUnit

import BitMask

import           Data.NineP
import qualified Data.NineP.MessageTypes as MT
import           Data.NineP.OpenMode
import           Data.NineP.Qid          hiding (Directory)
import qualified Data.NineP.Qid          as Qid
import           Data.NineP.Stat         hiding (AppendOnly,
                                          Directory)
import qualified Data.NineP.Stat         as Stat

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
tests socket = do
  testGroup
    "Server"
    [ testCase "testVersion01" (testVersion01 socket)
    , testCase "testVersion02" (testVersion02 socket)
    , testCase "testVersion03" (testVersion03 socket)
    , testCase "testVersion04" (testVersion04 socket)
    , testCase "testAttach01" (testAttach01 socket)
    , testCase "testStat01" (testStat01 socket)
    , testCase "testStatWriteOnlyFile" (testStatWriteOnlyFile socket)
    , testCase "testStatReadOnlyFile" (testStatReadOnlyFile socket)
    , testCase "testWalk01" (testWalk01 socket)
    , testCase "testClunk01" (testClunk01 socket)
    , testCase "testWalk02" (testWalk02 socket)
    , testCase "testWalk03" (testWalk03 socket)
    , testCase "testWalk04" (testWalk04 socket)
    , testCase "testWalk05" (testWalk05 socket)
    , testCase "testOpenWrite" (testOpenWrite socket)
    , testCase "testOpenRead" (testOpenRead socket)
    , testCase "testReadDirectoryDir1" (testReadDirectoryDir1 socket)
    , testCase "testReadDirectoryRoot" (testReadDirectoryRoot socket)
    , testCase "testWrite01" (testWrite01 socket)
    , testCase "testWrite02" (testWrite02 socket)
    ]

--     , testCase "testClunk02" (testClunk02 socket)
testContext :: Context ()
testContext = def {cFSItems = testFSItemsList}

testFSItemsList :: V.Vector (FSItem (Context u))
testFSItemsList =
  V.fromList
    [ directory "/" 0
    , writeOnlyFile "/in" 1
    , readOnlyFile "/out" 2
    , directory "/dir1" 3
    , writeOnlyFile "/dir1/in" 4
    , readOnlyFile "/dir1/out" 5
    ]

sendMessageWithTag
  :: ToNinePFormat a
  => Socket -> Tag -> a -> IO ()
sendMessageWithTag socket tag msg = send socket (toNinePFormat msg tag)

sendMessage
  :: ToNinePFormat a
  => Socket -> a -> IO ()
sendMessage socket msg = send socket (toNinePFormat msg 1)

getResponseMessageHeaders :: Get (Int, MT.ResponseMessageType, Tag)
getResponseMessageHeaders = do
  size <- getWord32le
  msgType <- getWord8
  tag <- getWord16le
  return (fromIntegral size, MT.MkResponseMessageType msgType, tag)

receiveAndCheckMessage
  :: (Serialize a, ToNinePFormat a, Eq a, Show a)
  => Socket -> a -> Assertion
receiveAndCheckMessage socket msg = do
  maybeResult <- recv socket 8192
  case maybeResult of
    Nothing ->
      assertFailure "receiveAndCheckMessage: Nothing received from the socket"
    Just result -> do
      when
        (BS.length result > 7)
        (case runGet get (BS.drop 7 result) of
           Left s        -> assertFailure s
           Right message -> message @?= msg)
      let eitherResult = runGet getResponseMessageHeaders result
      case eitherResult of
        Left s -> assertFailure s
        Right (size, msgType, tag) -> do
          tag @?= 1
          size @?= BS.length (toNinePFormat msg 1)

testVersion01 :: Socket -> Assertion
testVersion01 socket = do
  sendMessage socket (Tversion 8192 Ver9P2000)
  receiveAndCheckMessage socket (Rversion 8192 Ver9P2000)

testVersion02 :: Socket -> Assertion
testVersion02 socket = do
  sendMessage socket (Tversion 8192 VerUnknown)
  receiveAndCheckMessage socket (Rversion 8192 VerUnknown)

testVersion03 :: Socket -> Assertion
testVersion03 socket = do
  sendMessage socket (Tversion 9000 Ver9P2000)
  receiveAndCheckMessage socket (Rversion 9000 Ver9P2000)

testVersion04 :: Socket -> Assertion
testVersion04 socket = do
  sendMessage socket (Tversion 8000 Ver9P2000)
  receiveAndCheckMessage socket (Rversion 9000 Ver9P2000)

testAttach01 :: Socket -> Assertion
testAttach01 socket = do
  sendMessage socket (Tattach 0 0xffffffff "root" "")
  receiveAndCheckMessage socket (Rattach (Qid [Qid.Directory] 0 0))

testStat01 :: Socket -> Assertion
testStat01 socket = do
  sendMessage socket (Tstat 0)
  receiveAndCheckMessage
    socket
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

testWalk01 :: Socket -> Assertion
testWalk01 socket = do
  sendMessage socket (Twalk 0 1 [])
  receiveAndCheckMessage socket (Rwalk [])

testClunk01 :: Socket -> Assertion
testClunk01 socket = do
  sendMessage socket (Tclunk 1)
  receiveAndCheckMessage socket Rclunk

testWalk02 :: Socket -> Assertion
testWalk02 socket = do
  sendMessage socket (Twalk 0 1 ["/"])
  receiveAndCheckMessage socket (Rwalk [])
  sendMessage socket (Tclunk 1)
  receiveAndCheckMessage socket Rclunk

testWalk03 :: Socket -> Assertion
testWalk03 socket = do
  sendMessage socket (Twalk 0 1 ["dir1"])
  receiveAndCheckMessage socket (Rwalk [Qid [Qid.Directory] 0 3])
  sendMessage socket (Tclunk 1)
  receiveAndCheckMessage socket Rclunk

testWalk04 :: Socket -> Assertion
testWalk04 socket = do
  sendMessage socket (Twalk 0 1 ["dir1", "in"])
  receiveAndCheckMessage
    socket
    (Rwalk [Qid [Qid.Directory] 0 3, Qid [AppendOnly] 0 4])
  sendMessage socket (Tclunk 1)
  receiveAndCheckMessage socket Rclunk

testWalk05 :: Socket -> Assertion
testWalk05 socket = do
  sendMessage socket (Twalk 0 1 ["dir1"])
  receiveAndCheckMessage socket (Rwalk [Qid [Qid.Directory] 0 3])
  sendMessage socket (Twalk 1 2 ["in"])
  receiveAndCheckMessage socket (Rwalk [Qid [AppendOnly] 0 4])
  sendMessage socket (Tclunk 2)
  receiveAndCheckMessage socket Rclunk
  sendMessage socket (Tclunk 1)
  receiveAndCheckMessage socket Rclunk

testOpenWrite :: Socket -> Assertion
testOpenWrite socket = do
  sendMessage socket (Twalk 0 1 ["in"])
  receiveAndCheckMessage socket (Rwalk [Qid [AppendOnly] 0 1])
  sendMessage socket (Topen 1 Write)
  receiveAndCheckMessage socket (Ropen (Qid [AppendOnly] 0 1) 8977)
  sendMessage socket (Tclunk 1)
  receiveAndCheckMessage socket Rclunk

testOpenRead :: Socket -> Assertion
testOpenRead socket = do
  sendMessage socket (Twalk 0 1 ["out"])
  receiveAndCheckMessage socket (Rwalk [Qid [] 0 2])
  sendMessage socket (Topen 1 Read)
  receiveAndCheckMessage socket (Ropen (Qid [] 0 2) 8977)
  sendMessage socket (Tclunk 1)
  receiveAndCheckMessage socket Rclunk

-- testClunk02 :: Socket -> Assertion
-- testClunk02 socket = do
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
testReadDirectoryDir1 :: Socket -> Assertion
testReadDirectoryDir1 socket = do
  sendMessage socket (Twalk 0 1 ["dir1"])
  receiveAndCheckMessage socket (Rwalk [Qid [Qid.Directory] 0 3])
  sendMessage socket (Topen 1 Read)
  receiveAndCheckMessage socket (Ropen (Qid [Qid.Directory] 0 3) 8977)
  sendMessage socket (Tread 1 0 8168)
  let statBS = runPut . put . dStat . fDetails
  receiveAndCheckMessage
    socket
    ((Rread . BS.concat . fmap statBS)
       [writeOnlyFile "/dir1/in" 4, readOnlyFile "/dir1/out" 5])
  sendMessage socket (Tclunk 1)
  receiveAndCheckMessage socket Rclunk

testReadDirectoryRoot :: Socket -> Assertion
testReadDirectoryRoot socket = do
  sendMessage socket (Twalk 0 1 [])
  receiveAndCheckMessage socket (Rwalk [])
  sendMessage socket (Topen 1 Read)
  receiveAndCheckMessage socket (Ropen (Qid [Qid.Directory] 0 0) 8977)
  sendMessage socket (Tread 1 0 8168)
  let statBS = runPut . put . dStat . fDetails
  receiveAndCheckMessage
    socket
    ((Rread . BS.concat . fmap statBS)
       [writeOnlyFile "/in" 1, readOnlyFile "/out" 2, directory "/dir1" 3])
  sendMessage socket (Tclunk 1)
  receiveAndCheckMessage socket Rclunk

testStatWriteOnlyFile :: Socket -> Assertion
testStatWriteOnlyFile socket = do
  sendMessage socket (Twalk 0 1 ["in"])
  receiveAndCheckMessage socket (Rwalk [Qid [AppendOnly] 0 1])
  sendMessage socket (Tstat 1)
  receiveAndCheckMessage
    socket
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
  sendMessage socket (Tclunk 1)
  receiveAndCheckMessage socket Rclunk

testStatReadOnlyFile :: Socket -> Assertion
testStatReadOnlyFile socket = do
  sendMessage socket (Twalk 0 1 ["out"])
  receiveAndCheckMessage socket (Rwalk [Qid [] 0 2])
  sendMessage socket (Tstat 1)
  receiveAndCheckMessage
    socket
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
  sendMessage socket (Tclunk 1)
  receiveAndCheckMessage socket Rclunk

testWrite01 :: Socket -> Assertion
testWrite01 socket = do
  sendMessage socket (Twalk 0 1 ["out"])
  receiveAndCheckMessage socket (Rwalk [Qid [] 0 2])
  sendMessage socket (Topen 1 Read)
  receiveAndCheckMessage socket (Ropen (Qid [] 0 2) 8977)
  sendMessage socket (Twalk 0 2 ["in"])
  receiveAndCheckMessage socket (Rwalk [Qid [AppendOnly] 0 1])
  sendMessage socket (Topen 2 Write)
  receiveAndCheckMessage socket (Ropen (Qid [AppendOnly] 0 1) 8977)
  let writeContents = "testing write" :: BS.ByteString
  sendMessage socket (Twrite 2 0 writeContents)
  receiveAndCheckMessage
    socket
    (Rwrite (fromIntegral (BS.length writeContents)))
  sendMessage socket (Tread 1 0 (fromIntegral (BS.length writeContents)))
  receiveAndCheckMessage socket (Rread writeContents)
  sendMessage socket (Tclunk 1)
  receiveAndCheckMessage socket Rclunk
  sendMessage socket (Tclunk 2)
  receiveAndCheckMessage socket Rclunk

testWrite02 :: Socket -> Assertion
testWrite02 socket = do
  sendMessage socket (Twalk 0 1 ["out"])
  receiveAndCheckMessage socket (Rwalk [Qid [] 0 2])
  sendMessage socket (Topen 1 Read)
  receiveAndCheckMessage socket (Ropen (Qid [] 0 2) 8977)
  sendMessage socket (Twalk 0 2 ["in"])
  receiveAndCheckMessage socket (Rwalk [Qid [AppendOnly] 0 1])
  sendMessage socket (Topen 2 Write)
  receiveAndCheckMessage socket (Ropen (Qid [AppendOnly] 0 1) 8977)
  let writeContents = "testing write" :: BS.ByteString
  sendMessage socket (Twrite 2 0 writeContents)
  receiveAndCheckMessage
    socket
    (Rwrite (fromIntegral (BS.length writeContents)))
  sendMessage socket (Tread 1 0 (fromIntegral (BS.length writeContents)))
  receiveAndCheckMessage socket (Rread writeContents)
  let writeContentsAgain = "testing write again" :: BS.ByteString
  sendMessage socket (Twrite 2 0 writeContentsAgain)
  receiveAndCheckMessage
    socket
    (Rwrite (fromIntegral (BS.length writeContentsAgain)))
  sendMessage socket (Tread 1 0 (fromIntegral (BS.length writeContentsAgain)))
  receiveAndCheckMessage socket (Rread writeContentsAgain)
  sendMessage socket (Tclunk 1)
  receiveAndCheckMessage socket Rclunk
  sendMessage socket (Tclunk 2)
  receiveAndCheckMessage socket Rclunk
