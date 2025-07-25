module HsBindgen.Frontend.ProcessIncludes (
    GetMainHeader
  , processIncludes
    -- * Auxiliary
  , getIncludeTo
  , ifFromRootHeader_
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import HsBindgen.C.Predicate (IsMainHeader)
import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.RootHeader (RootHeader)
import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Process includes.

  == Context

  When the user invokes @hs-bindgen@, they provide us with one or more headers
  to process; we refer to these as the "main headers". In order to process
  these, we construct a new "root header" (see "HsBindgen.Frontend.RootHeader"),
  which has one @#include@ per user-specified main header. For example, the root
  header might look like

  > #include <a.h>
  > #include "b.h"

  These paths must be interpreted with respect to the @C_INCLUDE_PATH@, the @-I@
  command line options, etc.; we use 'CHeaderIncludePath' for this concept.

  == Selecting declarations

  When we see a declaration in the @clang@ AST, we might need to check if this
  declaration is from one of these main headers (as opposed to a header
  /included by/ one of the main headers). Unfortunately, @clang@ does not
  give us a 'CHeaderIncludePath' for the declaration, but rather a 'SourcePath'.
  The exact nature of this 'SourcePath' is a @clang@ internal detail, but it
  might for example be @/the/full/path/to/b.h@.

  In this example, we then /know/ that the set of main headers is @<a.h>@ and
  @"b.h"@, and we need to check if the 'SourcePath' @/the/full/path/to/b.h@
  happens to correspond to one of these main headers. Simply comparing the
  @basename@ is insufficient: it's entirely possible that for example both @b.h@
  and @internal/b.h@ exist in the library (or indeed, this particular @b.h@
  might be from a different library altogether).

  Therefore we need a /mapping/ from 'CHeaderIncludePath' to 'SourcePath', at
  least for the includes in the root header. The only reliable way that we found
  to get this mapping is by looking at how @clang@ resolves these headers as it
  parses the root header (there is an API specifically for resolving header
  paths, but it does subtly different things than the @clang@ parser does).

  Unfortunately, this means that we need to process all includes /prior/ to
  processing the rest of the AST, because we are not guaranteed to see the
  include of @b.h@ from the root header prior to processing it: this will
  /usually/ be the case, but not if @<a.h>@ /itself/ also includes @"b.h"@.

  == Setting the current main header

  When we see a function declaration, we must associate that function
  declaration with one of the main headers (so that we can generate the correct
  @#include@ when producing code for that function). It's not entirely obvious
  if we should use a 'CHeaderIncludePath' or a 'SourcePath' for this purpose;
  we currently choose the former, so that we can generate the somewhat cleaner
  lookling

  > foreign import capi "b.h foo" ..

  rather than

  > foreign import capi "/full/path/to/b.h foo" ..

  Arguably, the lattter is more correct, but the former is perhaps a little more
  portable (although @hs-bindgen@ is in general not intended to produce portable
  code anyway).

  When we see the @#include@ in the root header, @clang@ again only gives us
  a 'SourcePath' for the file-to-be-included. We ignore this, and instead use
  its /location/ as an index into the root header.

  (Note that we cannot really build a map from 'SourcePath' to
  'CHeaderIncludePath': multiple 'CHeaderIncludePath's in the root header could
  in principle resolve to the /same/ 'SourcePath.')
-------------------------------------------------------------------------------}

-- | Function to get the main header that (transitively) includes a source path
type GetMainHeader = SourcePath -> CHeaderIncludePath

-- | Process includes
--
-- We do this as separate pass over the clang AST; this should be relatively
-- cheap, as we can reuse the same 'CXTranslationUnit'.
processIncludes ::
     RootHeader
  -> CXTranslationUnit
  -> IO (IncludeGraph, IsMainHeader, GetMainHeader)
processIncludes rootHeader unit = do
    root     <- clang_getTranslationUnitCursor unit
    includes <- HighLevel.clang_visitChildren root $ simpleFold $ \curr -> do
                  mKind <- fromSimpleEnum <$> clang_getCursorKind curr
                  case mKind of
                    Right CXCursor_InclusionDirective -> do
                      include <- processInclude rootHeader curr
                      foldContinueWith include
                    _otherwise ->
                      foldContinue

    let includeGraph :: IncludeGraph
        includeGraph = IncludeGraph.fromList $
          map (\Include{..} -> (includeFrom, includeTo)) includes

        mainPathPairs :: [(SourcePath, CHeaderIncludePath)]
        mainPathPairs =
          mapMaybe (\Include{..} -> (includeTo,) <$> includeRoot) includes

        mainPathMap :: Map SourcePath CHeaderIncludePath
        mainPathMap = Map.fromList mainPathPairs

        mainPaths :: Set SourcePath
        mainPaths = Map.keysSet mainPathMap

        isMainHeader :: IsMainHeader
        isMainHeader loc = singleLocPath loc `Set.member` mainPaths

        getMainHeader :: GetMainHeader
        getMainHeader = case mainPathPairs of
          -- If we only have one main header, always return it without checking.
          [(_path, header)] -> const header
          _otherwise -> \path ->
            fromMaybe (panicPure ("getMainHeader failed for " ++ show path)) $
              (`Map.lookup` mainPathMap)
                =<< IncludeGraph.getMainPath mainPaths includeGraph path

    return (includeGraph, isMainHeader, getMainHeader)

{-------------------------------------------------------------------------------
  Process inclusion directives
-------------------------------------------------------------------------------}

-- | Include directive
--
-- Suppose we have file @a.h@ containing
--
-- > #include "b.h"
---
-- Then
--
-- * 'includeFrom' will be @/full/path/to/a.h@
-- * 'includeTo'   will be @/full/path/to/b.h@
-- * 'includeRoot' will be @"b.h"@ (the exact path as the user wrote it)
--
-- The full 'SourcePath's are constructed by @libclang@, and depend on factors
-- such as @-I@ command line arguments, environment variables such as
-- @C_INCLUDE_PATH@, etc.
--
-- We construct 'includeRoot' only for includes from the root header.
data Include = Include {
      includeFrom :: SourcePath
    , includeTo   :: SourcePath
    , includeRoot :: Maybe CHeaderIncludePath
    }

processInclude :: RootHeader -> CXCursor -> IO Include
processInclude rootHeader curr = do
   includeFrom <- getIncludeFrom curr
   includeTo   <- getIncludeTo   curr
   includeRoot <- ifFromRootHeader rootHeader curr return
   return Include{..}

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

getIncludeFrom :: MonadIO m => CXCursor -> m SourcePath
getIncludeFrom curr =
    singleLocPath <$> HighLevel.clang_getCursorLocation' curr

getIncludeTo :: MonadIO m => CXCursor -> m SourcePath
getIncludeTo curr = do
    file <- clang_getIncludedFile curr
    SourcePath <$> clang_getFileName file

ifFromRootHeader ::
     MonadIO m
  => RootHeader -> CXCursor -> (CHeaderIncludePath -> m r) -> m (Maybe r)
ifFromRootHeader rootHeader curr k = do
    isMain <- clang_Location_isFromMainFile =<< clang_getCursorLocation curr
    if isMain then do
      loc <- HighLevel.clang_getCursorLocation' curr
      Just <$> k (rootHeader `RootHeader.at` loc)
    else return Nothing

ifFromRootHeader_ ::
     MonadIO m
  => RootHeader -> CXCursor -> (CHeaderIncludePath -> m ()) -> m ()
ifFromRootHeader_ rootHeader curr k = void $ ifFromRootHeader rootHeader curr k
