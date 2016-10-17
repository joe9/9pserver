
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- The exceptions one would want to throw to be understood by the existing 9P clients.

module Network.NineP.Error
    ( NineError(..)
    , showNineError
    ) where

import Protolude hiding (concat)
import Control.Exception
import Data.Typeable
import Data.Word
import Data.ByteString
import Data.Serialize
import Data.String.Conversions

data NineError =
    ENotImplemented ByteString |
    ENotADir |
    EDir |
    ENoFile ByteString |
    ENoFid Word32 |
    ENoAuthRequired |
    EPermissionDenied |
    EInval |
    OtherError ByteString deriving (Typeable)

-- instance Exception NineError

-- TODO rename this to nineErrorToByteString or show b or some such
-- |See also: @linux\/net\/9p\/error.c@
showNineError :: NineError -> ByteString
showNineError (ENotImplemented s) = append s " is not implemented"
showNineError ENotADir = "not a directory"
showNineError EDir = "Is a directory"
showNineError (ENoFile _) = "file not found"
showNineError (ENoFid i) = concat [runPut (putWord32le i), " is not registered on the server"]
showNineError ENoAuthRequired = "the server doesn't require any kind of authentication"
showNineError EPermissionDenied = "permission denied"
showNineError EInval = "Invalid argument"
showNineError (OtherError s) = s
