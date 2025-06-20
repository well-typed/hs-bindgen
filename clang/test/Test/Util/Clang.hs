{-# LANGUAGE OverloadedStrings #-}

-- | Convenience functions around the clang bindings
--
-- Intended for unqualified import.
module Test.Util.Clang (
    -- * Top-level call into clang
    withInput
  , parseUsing
  ) where

import Control.Exception
import Data.Default

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import Test.Util.Input (TestInput(..))

{-------------------------------------------------------------------------------
  Top-level call into clang
-------------------------------------------------------------------------------}

withInput :: TestInput -> (CXTranslationUnit -> IO a) -> IO a
withInput (TestInput input) onSuccess =
    HighLevel.withUnsavedFile "test.h" input $ \file ->
    HighLevel.withIndex DisplayDiagnostics   $ \ix   ->
    HighLevel.withTranslationUnit2
      ix
      (Just "test.h")
      def
      [file]
      mempty
      onFailure
      onSuccess
  where
    onFailure :: SimpleEnum CXErrorCode -> IO a
    onFailure = throwIO . userError . show

parseUsing :: Fold IO a -> TestInput -> IO [a]
parseUsing fold input = do
    withInput input $ \unit -> do
      root   <- clang_getTranslationUnitCursor unit
      HighLevel.clang_visitChildren root fold
