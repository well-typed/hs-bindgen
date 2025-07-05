-- | Utilities for writing @tasty@ tests
--
-- Intended for unqualified import.
module Test.Common.Util.Tasty (
    -- * Assertions
    assertException
    -- * Golden tests
  , goldenVsStringDiff_
  ) where

import AnsiDiff (ansidiff)
import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as UTF8
import Data.Either (isRight)
import Data.Proxy

import Test.Tasty
import Test.Tasty.HUnit

import Test.Common.Util.Tasty.Golden

{-------------------------------------------------------------------------------
  Assertions
-------------------------------------------------------------------------------}

assertException :: forall e a.
     Exception e
  => String -> Proxy e -> IO a -> Assertion
assertException msg _ action = do
  result <- tryJust (\x -> fromException x :: Maybe e) action
  when (isRight result) $ assertFailure msg

{-------------------------------------------------------------------------------
  Golden tests
-------------------------------------------------------------------------------}

-- | Like goldenVsString but using our own diff function.
goldenVsStringDiff_ ::
     TestName
  -> FilePath
  -> ((String -> IO ()) -> IO String)
  -> TestTree
goldenVsStringDiff_ name fp action = goldenTestSteps name correct action cmp update
  where
    correct :: IO String
    correct = do
        contents <- BS.readFile fp
        return $ UTF8.toString contents

    update :: String -> IO ()
    update s = BS.writeFile fp (UTF8.fromString s)

    cmp :: String -> String -> IO (Maybe String)
    cmp xss yss
        | xss == yss = return Nothing
        | otherwise  = return $ Just $ ansidiff xss yss

