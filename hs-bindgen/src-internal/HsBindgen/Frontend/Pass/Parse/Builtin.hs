module HsBindgen.Frontend.Pass.Parse.Builtin (
    CursorSource(..)
  , getCursorSource
  , checkIsBuiltin
  ) where

import Foreign.C (CUInt)

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C

-- | Where a cursor is defined
data CursorSource =
    -- | Built into Clang, such as @__INT_MAX__@
    SourceBuiltin
    -- | A declaration; for a @-D@ option, at its presumed line and column
  | SourceDecl (SingleLoc C.DeclPath)

getCursorSource :: MonadIO m => CXCursor -> m CursorSource
getCursorSource curr = getExpansion curr >>= \case
    ExpansionBuiltin         -> pure SourceBuiltin
    ExpansionCommandLine loc -> pure $ SourceDecl loc
    ExpansionFile loc        -> SourceDecl <$> traverse declPath loc
  where
    declPath :: MonadIO m => CXFile -> m C.DeclPath
    declPath file = do
      name <- SourcePath <$> clang_getFileName file
      if RootHeader.isRootHeaderPath name
        then pure C.InRootHeader
        else C.InHeader <$> HighLevel.clang_getRealPath file

-- | Check for built-in definitions
--
-- Agrees with 'getCursorSource' returning 'SourceBuiltin', and exists only for
-- speed: unlike 'getCursorSource', it does not resolve the location of an
-- ordinary declaration, which the type parser, visiting every type reference,
-- would discard.
checkIsBuiltin :: MonadIO m => CXCursor -> m (Maybe Text)
checkIsBuiltin curr = getExpansion curr >>= \case
    ExpansionBuiltin -> Just <$> clang_getCursorSpelling curr
    _otherwise       -> return Nothing

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Presumed file name of a @-D@ option, chosen by Clang
commandLineName :: SourcePath
commandLineName = SourcePath "<command line>"

-- | Expansion location of a cursor
--
--Internal! (May contain a pointer 'CXFile' that is freed when the translation
--unit goes).
data Expansion =
    -- | In Clang's predefines buffer, as a built-in
    ExpansionBuiltin
    -- | In Clang's predefines buffer, from a @-D@ option
  | ExpansionCommandLine (SingleLoc C.DeclPath)
    -- | In a file; careful; contains a pointer ('CXFile') that becomes invalid
    -- when the translation unit goes
  | ExpansionFile (SingleLoc CXFile)

getExpansion :: MonadIO m => CXCursor -> m Expansion
getExpansion curr = do
    start <- clang_getCursorLocation curr
    (file, line, col, offset) <- clang_getExpansionLocation start
    if not (isNullPtr file)
      then return $ ExpansionFile $ mkLoc file line col offset
      else do
        -- Built-ins and @-D@ options both live in the predefines buffer, which
        -- has no file; only the presumed file name tells them apart.
        (path, pLine, pCol) <- clang_getPresumedLocation start
        return $
          if SourcePath path == commandLineName
            then ExpansionCommandLine $ mkLoc C.OnCommandLine pLine pCol offset
            else ExpansionBuiltin
  where
    mkLoc :: path -> CUInt -> CUInt -> CUInt -> SingleLoc path
    mkLoc path line col offset = SingleLoc{
          singleLocPath   = path
        , singleLocLine   = fromIntegral line
        , singleLocColumn = fromIntegral col
        , singleLocOffset = fromIntegral offset
        }
