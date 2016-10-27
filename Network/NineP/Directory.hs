{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.NineP.Directory where

-- import           Protolude
import           System.Posix.ByteString.FilePath

import Network.NineP.Context
import Network.NineP.Functions

-- convenience function to create a directory FSItem
directory :: RawFilePath -> FSItemsIndex -> FSItem (Context u)
directory name index = FSItem Occupied (dirDetails name index) []

