{-# LANGUAGE RecordWildCards #-}

module HsBindgen.Clang.HighLevel.Wrappers (
    withIndex
  , withTranslationUnit
  , withTranslationUnit2
  , withUnsavedFile
  ) where

import Control.Exception (bracket)
import Foreign.C.String (withCString, withCStringLen)
import GHC.Stack (HasCallStack)

import HsBindgen.Clang.Args
import HsBindgen.Clang.Enum.Bitfield
import HsBindgen.Clang.Enum.Simple
import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Clang.Paths

-- | Brackets 'clang_createIndex' with 'clang_disposeIndex'
withIndex ::
     DisplayDiagnostics
  -> (CXIndex -> IO a)
  -> IO a
withIndex diagnostics =
    bracket (clang_createIndex diagnostics) clang_disposeIndex

-- | Brackets 'clang_parseTranslationUnit' with 'clang_disposeTranslationUnit'
withTranslationUnit ::
     HasCallStack
  => CXIndex
  -> SourcePath
  -> ClangArgs
  -> [CXUnsavedFile]
  -> BitfieldEnum CXTranslationUnit_Flags
  -> (CXTranslationUnit -> IO a)
  -> IO a
withTranslationUnit index src args unsavedFiles options =
    bracket
      (clang_parseTranslationUnit index src args unsavedFiles options)
      clang_disposeTranslationUnit

-- | Brackets 'clang_parseTranslationUnit2' with 'clang_disposeTranslationUnit'
withTranslationUnit2 ::
     HasCallStack
  => CXIndex
  -> SourcePath
  -> ClangArgs
  -> [CXUnsavedFile]
  -> BitfieldEnum CXTranslationUnit_Flags
  -> (Either (SimpleEnum CXErrorCode) CXTranslationUnit -> IO a)
  -> IO a
withTranslationUnit2 index src args unsavedFiles options =
    bracket (clang_parseTranslationUnit2 index src args unsavedFiles options) $
      \case
        Right unit -> clang_disposeTranslationUnit unit
        Left _err  -> return ()

-- | Constructs a 'CXUnsavedFile', allocating memory for the passed strings
withUnsavedFile :: String -> String -> (CXUnsavedFile -> IO a) -> IO a
withUnsavedFile filename contents f =
    withCString filename $ \cxUnsavedFileFilename ->
      withCStringLen contents $ \(cxUnsavedFileContents, len) ->
        let cxUnsavedFileLength = fromIntegral len
        in  f CXUnsavedFile{..}
