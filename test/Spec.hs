
module Main where

import           System.Posix.ByteString.FilePath
import           System.Posix.FilePath
import           Test.Tasty (defaultMain, testGroup)
import           Network.NineP.Context hiding (File)
import qualified Network.NineP.Context as Context
--
import qualified Response.Tests
import qualified Server.Tests
-- import qualified Keymap.CustomDvorak.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Response.Tests.tests
    , Server.Tests.tests
    ]
