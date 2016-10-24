{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Response.Tests
  ( tests
  ) where

import Data.Bits
import Data.Default
import Protolude
import Test.Tasty
import Test.Tasty.HUnit

--
import BitMask

--
import           Data.NineP
import           Data.NineP.Qid hiding (Directory)
import qualified Data.NineP.Qid as Qid
import           Data.NineP.Stat  hiding (Directory)
import qualified Data.NineP.Stat  as Stat

--
import Network.NineP
import Network.NineP.Context
import Network.NineP.Functions
import Network.NineP.Error
import Network.NineP.Response
import Network.NineP.Server

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
    , testCase "testClunk01" testClunk01
    , testCase "testWalk01" testWalk01
    , testCase "testWalk02" testWalk02
    , testCase "testWalk03" testWalk03
    , testCase "testWalk04" testWalk04
    , testCase "testWalk05" testWalk05
    ]

testVersion01 :: Assertion
testVersion01 =
  let result = version (Tversion 8192 Ver9P2000) sampleContext
  in (fst result) @?= (Right (Rversion 8192 Ver9P2000))

testVersion02 :: Assertion
testVersion02 =
  let result = version (Tversion 8192 VerUnknown) sampleContext
  in (fst result) @?= (Right (Rversion 8192 VerUnknown))

testVersion03 :: Assertion
testVersion03 =
  let result = version (Tversion 9000 Ver9P2000) sampleContext
  in (fst result) @?= (Right (Rversion 9000 Ver9P2000))

testVersion04 :: Assertion
testVersion04 =
  let result = version (Tversion 8000 Ver9P2000) sampleContext
  in (fst result) @?= (Right (Rversion 8192 Ver9P2000))

testAttach01 :: Assertion
testAttach01 =
  let result = attach (Tattach 0 0xffffffff "root" "") sampleContext
  in (fst result) @?= (Right (Rattach (Qid [Qid.Directory] 0 0)))

testStat01 :: Assertion
testStat01 =
  let attachresult = attach (Tattach 0 0xffffffff "root" "") sampleContext
      result = stat (Tstat 0) (snd attachresult)
  in fst result @?=
     Right
       (Rstat
        { rsStat =
            Stat
            { stTyp = 0
            , stDev = 0
            , stQid =
                Qid
                { qType = [Qid.Directory]
                , qversion = 0
                , qPath = 0
                }
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
testClunk01 =
  let attachresult = attach (Tattach 0 0xffffffff "root" "") sampleContext
      statresult = stat (Tstat 0) (snd attachresult)
      result = clunk (Tclunk 0) (snd statresult)
  in fst result @?= Right Rclunk

testWalk01 :: Assertion
testWalk01 =
  let attachresult = attach (Tattach 0 0xffffffff "root" "") sampleContext
      statresult = stat (Tstat 0) (snd attachresult)
      result = walk (Twalk 0 1 []) (snd statresult)
  in fst result @?= Right (Rwalk [])

testWalk02 :: Assertion
testWalk02 =
  let attachresult = attach (Tattach 0 0xffffffff "root" "") sampleContext
      statresult = stat (Tstat 0) (snd attachresult)
      result = walk (Twalk 0 1 ["/"]) (snd statresult)
  in fst result @?= Right (Rwalk [])

testWalk03 :: Assertion
testWalk03 =
  let attachresult = attach (Tattach 0 0xffffffff "root" "") sampleContext
      statresult = stat (Tstat 0) (snd attachresult)
      result = walk (Twalk 0 1 ["dir1"]) (snd statresult)
  in fst result @?= Right (Rwalk [Qid [Qid.Directory] 0 3])

testWalk04 :: Assertion
testWalk04 =
  let attachresult = attach (Tattach 0 0xffffffff "root" "") sampleContext
      statresult = stat (Tstat 0) (snd attachresult)
      result = walk (Twalk 0 1 ["dir1","in"]) (snd statresult)
  in fst result @?= Right (Rwalk [Qid [Qid.Directory] 0 3, Qid [Qid.File,AppendOnlyFile] 0 4])

testWalk05 :: Assertion
testWalk05 =
  let attachresult = attach (Tattach 0 0xffffffff "root" "") sampleContext
      statresult = stat (Tstat 0) (snd attachresult)
      walkresult = walk (Twalk 0 1 ["dir1"]) (snd statresult)
      result = walk (Twalk 1 2 ["in"]) (snd walkresult)
  in fst result @?= Right (Rwalk [Qid [Qid.File,AppendOnlyFile] 0 4])

-- testIdentifyStateChanges02 :: Assertion
-- testIdentifyStateChanges02 =
--   (identifyStateChanges (def {sDepressedModifiers = 1}) def) @?=
--   UpdatedStateComponents 0b1
-- testIdentifyStateChanges03 :: Assertion
-- testIdentifyStateChanges03 =
--   (identifyStateChanges (def {sLatchedModifiers = 1}) def) @?=
--   UpdatedStateComponents 0b10
-- testIdentifyStateChanges04 :: Assertion
-- testIdentifyStateChanges04 =
--   (identifyStateChanges (def {sLockedModifiers = 1}) def) @?=
--   UpdatedStateComponents 0b100
-- testIdentifyStateChanges05 :: Assertion
-- testIdentifyStateChanges05 =
--   (identifyStateChanges (def {sEffectiveModifiers = 1}) def) @?=
--   UpdatedStateComponents 0b1000
-- testIdentifyStateChanges06 :: Assertion
-- testIdentifyStateChanges06 =
--   (identifyStateChanges (def {sDepressedGroup = 1}) def) @?=
--   UpdatedStateComponents 0b10000
-- testIdentifyStateChanges07 :: Assertion
-- testIdentifyStateChanges07 =
--   (identifyStateChanges (def {sLatchedGroup = 1}) def) @?=
--   UpdatedStateComponents 0b100000
-- testIdentifyStateChanges08 :: Assertion
-- testIdentifyStateChanges08 =
--   (identifyStateChanges (def {sLockedGroup = 1}) def) @?=
--   UpdatedStateComponents 0b1000000
-- testIdentifyStateChanges09 :: Assertion
-- testIdentifyStateChanges09 =
--   (identifyStateChanges (def {sEffectiveGroup = 1}) def) @?=
--   UpdatedStateComponents 0b10000000
-- testIdentifyStateChanges10 :: Assertion
-- testIdentifyStateChanges10 =
--   (identifyStateChanges (def {sEffectiveGroup = 1}) def) @?=
--   UpdatedStateComponents 0b10000000
-- -- a keycode = 38, keysymbol = 0x61
-- -- using the default void keymap, should return 0
-- testOnKeyPress01 :: Assertion
-- testOnKeyPress01 = (onKeyPress 38 def) @?= (UpdatedStateComponents 0, def)
-- -- using the CustomDvorak keymap
-- testOnKeyPress02 :: Assertion
-- testOnKeyPress02 =
--   (onKeyPress 38 (pickInitialState 1)) @?=
--   (UpdatedStateComponents 0, pickInitialState 1)
-- testOnKeyPress03 :: Assertion
-- testOnKeyPress03 =
--   (onKeyPress 38 (pickInitialState 1)) @?=
--   (UpdatedStateComponents 0, pickInitialState 1)
-- -- o keycode 39, keysymbol 0x6f
-- testOnKeyPress04 :: Assertion
-- testOnKeyPress04 =
--   (onKeyPress 39 (pickInitialState 1)) @?=
--   (UpdatedStateComponents 0, pickInitialState 1)
-- -- q keycode 53, keysymbol 0x71
-- testOnKeyPress05 :: Assertion
-- testOnKeyPress05 =
--   (onKeyPress 53 (pickInitialState 1)) @?=
--   (UpdatedStateComponents 0, pickInitialState 1)
-- -- Escape keycode 9, keysymbol 0xff1b
-- testOnKeyPress06 :: Assertion
-- testOnKeyPress06 =
--   (onKeyPress 9 (pickInitialState 1)) @?=
--   (UpdatedStateComponents 0, pickInitialState 1)
-- -- A keycode = 38, keysymbol = 0x41
-- testOnKeyPress07 :: Assertion
-- testOnKeyPress07 =
--   let original =
--         (pickInitialState 1) {sDepressedModifiers = setModifier 0 Shift}
--       s = original
--       expected =
--         original
--         { sDepressedModifiers = setModifier 0 Shift
--         , sEffectiveModifiers = setModifier 0 Shift
--         }
--   in (onKeyPress 38 original) @?=
--      (identifyStateChanges original expected, expected)
-- -- Meta_L keycode = 64, keysymbol = Meta_L = 0xffe7 = 65,511 ==> Mod3
-- testOnKeyPress08 :: Assertion
-- testOnKeyPress08 =
--   let original = pickInitialState 1
--       s = original
--       expected =
--         original
--         { sDepressedModifiers = setModifier (sDepressedModifiers s) Mod3
--         , sEffectiveModifiers = setModifier (sEffectiveModifiers s) Mod3
--         , sLatchedModifiers = setModifier (sLatchedModifiers s) Mod3
--         }
--   in (onKeyPress 64 original) @?=
--      (identifyStateChanges original expected, expected)
-- -- Alt_L keycode = 108, keysymbol = Alt_L ==> Mod1
-- testOnKeyPress09 :: Assertion
-- testOnKeyPress09 =
--   let original = pickInitialState 1
--       s = original
--       expected =
--         original
--         { sDepressedModifiers = setModifier (sDepressedModifiers s) Mod1
--         , sEffectiveModifiers = setModifier (sEffectiveModifiers s) Mod1
--         , sLatchedModifiers = setModifier (sLatchedModifiers s) Mod1
--         }
--   in (onKeyPress 108 original) @?=
--      (identifyStateChanges original expected, expected)
-- -- Control_L keycode = 66, keysymbol = Control_L ==> Control
-- testOnKeyPress10 :: Assertion
-- testOnKeyPress10 =
--   let original = pickInitialState 1
--       s = original
--       expected =
--         original
--         { sDepressedModifiers = setModifier (sDepressedModifiers s) Control
--         , sEffectiveModifiers = setModifier (sEffectiveModifiers s) Control
--         , sLatchedModifiers = setModifier (sLatchedModifiers s) Control
--         }
--   in (onKeyPress 66 original) @?=
--      (identifyStateChanges original expected, expected)
-- -- Shift_L keycode = 50, keysymbol = Shift_L ==> Shift
-- testOnKeyPress11 :: Assertion
-- testOnKeyPress11 =
--   let original = pickInitialState 1
--       s = original
--       expected =
--         original
--         { sDepressedModifiers = setModifier (sDepressedModifiers s) Shift
--         , sEffectiveModifiers = setModifier (sEffectiveModifiers s) Shift
--         , sLatchedModifiers = setModifier (sLatchedModifiers s) Shift
--         }
--   in (onKeyPress 50 original) @?=
--      (identifyStateChanges original expected, expected)
-- -- the depressed Shift should not be released as per the workflow in notes
-- -- if there is just a latched Shift, it should be released
-- -- Escape keycode 9, keysymbol 0xff1b
-- testOnKeyRelease01 :: Assertion
-- testOnKeyRelease01 =
--   let original =
--         (pickInitialState 1) {sDepressedModifiers = setModifier 0 Shift}
--       expected = original {sEffectiveModifiers = setModifier 0 Shift}
--   in (onKeyRelease 9 original) @?= (UpdatedStateComponents 0, expected)
-- -- the Shift should not be released
-- -- A keycode 38, keysymbol 0x41
-- testOnKeyRelease02 :: Assertion
-- testOnKeyRelease02 =
--   let original =
--         (pickInitialState 1) {sDepressedModifiers = setModifier 0 Shift}
--       expected =
--         (pickInitialState 1)
--         { sDepressedModifiers = setModifier 0 Shift
--         , sEffectiveModifiers = setModifier 0 Shift
--         }
--   in (onKeyRelease 38 original) @?= (UpdatedStateComponents 0, expected)
-- -- the depressed Shift should not be released as per the workflow in notes
-- -- if there is just a latched Shift, it should be released
-- -- Escape keycode 9, keysymbol 0xff1b
-- testOnKeyRelease03 :: Assertion
-- testOnKeyRelease03 =
--   let original = (pickInitialState 1) {sLatchedModifiers = setModifier 0 Shift}
--       expected = pickInitialState 1
--       -- if there is just a latched Shift then it implies an Effective Shift
--       -- too, so both should be released
--   in (onKeyRelease 9 original) @?= (UpdatedStateComponents 0b1010, expected)
-- -- the Shift should not be released
-- -- A keycode 38, keysymbol 0x41
-- testShiftLevelAlphabet01 :: Assertion
-- testShiftLevelAlphabet01 =
--   let original =
--         (pickInitialState 1)
--         { sDepressedModifiers = setModifier 0 Shift
--         , sEffectiveModifiers = setModifier 0 Shift
--         , sLatchedModifiers = setModifier 0 Shift
--         }
--       expected = original
--   in (onKeyPress 38 original) @?=
--      (identifyStateChanges original expected, expected)
-- testShiftLevelAlphabet02 :: Assertion
-- testShiftLevelAlphabet02 =
--   let original =
--         (pickInitialState 1)
--         { sDepressedModifiers = setModifier 0 Shift
--         , sEffectiveModifiers = setModifier 0 Shift
--         , sLatchedModifiers = setModifier 0 Shift
--         }
--       expected = original
--   in (onKeyPress 38 original) @?=
--      (identifyStateChanges original expected, expected)
-- -- shift twice should become locked with sticky on
-- -- Shift_L keycode = 50, keysymbol = Shift_L ==> Shift
-- testStickyLocking01 :: Assertion
-- testStickyLocking01 =
--   let original =
--         (pickInitialState 1)
--         { sDepressedModifiers = setModifier 0 Shift
--         , sEffectiveModifiers = setModifier 0 Shift
--         , sLatchedModifiers = setModifier 0 Shift
--         }
--       s = original
--       expected =
--         original
--         { sDepressedModifiers = setModifier (sDepressedModifiers s) Shift
--         , sEffectiveModifiers = setModifier (sEffectiveModifiers s) Shift
--         , sLatchedModifiers = 0
--         , sLockedModifiers = setModifier 0 Shift
--         }
--   in (onKeyPress 50 original) @?=
--      (identifyStateChanges original expected, expected)
-- -- locked shift loses lock with sticky on
-- -- Shift_L keycode = 50, keysymbol = Shift_L ==> Shift
-- testStickyLocking02 :: Assertion
-- testStickyLocking02 =
--   let original =
--         (pickInitialState 1)
--         { sEffectiveModifiers = setModifier 0 Shift
--         , sLockedModifiers = setModifier 0 Shift
--         }
--       s = original
--       expected =
--         original
--         { sDepressedModifiers = setModifier 0 Shift
--         , sEffectiveModifiers = setModifier 0 Shift
--         , sLockedModifiers = clearModifier 0 Shift
--         }
--   in (onKeyPress 50 original) @?=
--      (identifyStateChanges original expected, expected)
-- -- locked shift stays there with sticky on even when key released
-- -- Shift_L keycode = 50, keysymbol = Shift_L ==> Shift
-- testStickyLocking03 :: Assertion
-- testStickyLocking03 =
--   let original =
--         (pickInitialState 1)
--         { sEffectiveModifiers = setModifier 0 Shift
--         , sLockedModifiers = setModifier 0 Shift
--         }
--       s = original
--       expected =
--         original
--         { sEffectiveModifiers = setModifier 0 Shift
--         , sLockedModifiers = setModifier 0 Shift
--         }
--   in (onKeyRelease 50 original) @?=
--      (identifyStateChanges original expected, expected)
-- -- latched shift stays there with sticky on even when key released
-- -- Shift_L keycode = 50, keysymbol = Shift_L ==> Shift
-- testStickyLocking04 :: Assertion
-- testStickyLocking04 =
--   let original =
--         (pickInitialState 1)
--         { sDepressedModifiers = setModifier 0 Shift
--         , sEffectiveModifiers = setModifier 0 Shift
--         , sLatchedModifiers = setModifier 0 Shift
--         }
--       s = original
--       expected =
--         (pickInitialState 1)
--         { sEffectiveModifiers = setModifier 0 Shift
--         , sLatchedModifiers = setModifier 0 Shift
--         }
--   in (onKeyRelease 50 original) @?=
--      (identifyStateChanges original expected, expected)
-- -- shift twice should just be depressed with sticky off
-- testNonStickyLatching01 :: Assertion
-- testNonStickyLatching01 =
--   let original =
--         (pickInitialState 0)
--         { sDepressedModifiers = setModifier 0 Shift
--         , sEffectiveModifiers = setModifier 0 Shift
--         }
--       s = original
--       expected =
--         original
--         { sDepressedModifiers = setModifier 0 Shift
--         , sEffectiveModifiers = setModifier 0 Shift
--         }
--   in (onKeyPress 50 original) @?=
--      (identifyStateChanges original expected, expected)
-- -- shift release should release the depressed and effective with sticky off
-- testNonStickyLatching02 :: Assertion
-- testNonStickyLatching02 =
--   let original =
--         (pickInitialState 0)
--         { sDepressedModifiers = setModifier 0 Shift
--         , sEffectiveModifiers = setModifier 0 Shift
--         }
--       s = original
--       expected =
--         original
--         { sDepressedModifiers = 0
--         , sEffectiveModifiers = 0
--         }
--   in (onKeyRelease 50 original) @?=
--      (identifyStateChanges original expected, expected)
