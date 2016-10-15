
module Network.NineP.Internal.Types where

import Data.Word
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text

type Offset = Word64

type Length = Word32

type ResultingData = B.ByteString

type WriteData = B.ByteString

type Fid = Word32

type AFid = Word32

type NewFid = Word32

type Permissions = Word32

type Mode = Word8

type UserName = Text

type AccessName = Text

type FileVersion = Word32 -- s for context including user state
