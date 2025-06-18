{-# LANGUAGE RecordWildCards #-}

module Clang.HighLevel.Wrappers (
    withIndex
  , withTranslationUnit
  , withTranslationUnit2
  , withUnsavedFile
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Foreign.C.String (withCString, withCStringLen)
import GHC.Stack (HasCallStack)

import Clang.Args
import Clang.Enum.Bitfield
import Clang.Enum.Simple
import Clang.LowLevel.Core
import Clang.Paths

-- | Brackets 'clang_createIndex' with 'clang_disposeIndex'
withIndex ::
     (MonadIO m, MonadMask m)
  => DisplayDiagnostics
  -> (CXIndex -> m a)
  -> m a
withIndex diagnostics =
    bracket (clang_createIndex diagnostics) clang_disposeIndex

-- | Brackets 'clang_parseTranslationUnit' with 'clang_disposeTranslationUnit'
withTranslationUnit ::
     (MonadIO m, MonadMask m, HasCallStack)
  => CXIndex
  -> Maybe SourcePath
  -> ClangArgs
  -> [CXUnsavedFile]
  -> BitfieldEnum CXTranslationUnit_Flags
  -> (CXTranslationUnit -> m a)
  -> m a
withTranslationUnit index src args unsavedFiles options =
    bracket
      (clang_parseTranslationUnit index src args unsavedFiles options)
      clang_disposeTranslationUnit

-- | Brackets 'clang_parseTranslationUnit2' with 'clang_disposeTranslationUnit'
withTranslationUnit2 ::
     (MonadIO m, MonadMask m, HasCallStack)
  => CXIndex
  -> Maybe SourcePath
  -> ClangArgs
  -> [CXUnsavedFile]
  -> BitfieldEnum CXTranslationUnit_Flags
  -> (SimpleEnum CXErrorCode -> m a) -- ^ Handle errors
  -> (CXTranslationUnit      -> m a)
  -> m a
withTranslationUnit2 index src args unsavedFiles options onFailure onSuccess =
    bracket
      (clang_parseTranslationUnit2 index src args unsavedFiles options)
      ( \case
          Right unit -> clang_disposeTranslationUnit unit
          Left _err  -> return ()
      )
      ( \case
          Right unit -> onSuccess unit
          Left  err  -> onFailure err
      )

-- | Constructs a 'CXUnsavedFile', allocating memory for the passed strings
--
-- 'cxUnsavedFileLength' is computed from the length of the contents.
withUnsavedFile ::
     String -- ^ Filename
  -> String -- ^ Contents
  -> (CXUnsavedFile -> IO a) -> IO a
withUnsavedFile filename contents f =
    withCString filename $ \cxUnsavedFileFilename ->
      withCStringLen contents $ \(cxUnsavedFileContents, len) ->
        let cxUnsavedFileLength = fromIntegral len
        in  f CXUnsavedFile{..}
