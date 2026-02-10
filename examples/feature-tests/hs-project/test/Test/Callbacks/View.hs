{-# LANGUAGE DataKinds #-}

module Test.Callbacks.View (
    tests
  ) where

import Control.Exception (bracket)
import Foreign.C.String
import System.Directory
import System.IO.Temp
import Test.Tasty
import Test.Tasty.QuickCheck

import HsBindgen.Runtime.PtrConst qualified as PtrConst

import Generated.Callbacks.View qualified as Types
import Generated.Callbacks.View.FunPtr qualified as FunPtr
import Generated.Callbacks.View.Safe qualified as Safe
import Test.Util.FunPtr qualified as Util

tests :: TestTree
tests = testGroup "Test.Callbacks.View" [
      testProperty "prop_view_file_count_lines" prop_view_file_count_lines
    ]

newtype FileContents = FileContents String
  deriving stock Show

instance Arbitrary FileContents where
  arbitrary = FileContents <$> listOf genCharOrNewline
    where
      genCharOrNewline :: Gen Char
      genCharOrNewline = frequency [
          (1, pure '\n')
        , (4, arbitrary)
        ]
  shrink (FileContents str) = FileContents <$> shrink str

prop_view_file_count_lines :: FileContents -> Property
prop_view_file_count_lines (FileContents str) = ioProperty $
    bracket (writeSystemTempFile "prop_view_file_count_lines" str) removeFile $ \filePath ->
    withCString filePath $ \cFilePath -> do
      writeFile filePath str
      numLines' <- Safe.view_file
                      (PtrConst.unsafeFromPtr cFilePath)
                      (Types.View_str $ Util.castFunPtrCoercible FunPtr.count_lines)
      pure
        $ tabulate "number of lines" [show numLines]
        $ numLines === fromIntegral numLines'
  where
    numLines = length $ filter (=='\n') str
