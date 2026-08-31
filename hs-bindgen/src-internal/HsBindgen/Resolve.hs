module HsBindgen.Resolve (
    -- * Trace messages
    ResolveHeaderMsg(..)
    -- * API
  , resolveHeaders
  ) where

import Control.Monad ((<=<))
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.List.Compat ((!?))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.SimplePrettyPrint (hang, hsep, string)

import Clang.Args
import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types (Fold, foldContinue, foldContinueWith, simpleFold)
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Clang
import HsBindgen.Errors (panicIO)
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data ResolveHeaderMsg =
    ResolveHeaderClang ClangMsg
  | ResolveHeaderFound C.HashIncludeArg RealPath
  | ResolveHeaderNotAttempted C.HashIncludeArg
  deriving stock (Show)

instance PrettyForTrace ResolveHeaderMsg where
  prettyForTrace = \case
    ResolveHeaderClang msg -> hang
      "During header resolution:" 2 (prettyForTrace msg)
    ResolveHeaderFound header path -> hsep [
        "Header"
      , string header.path
      , "resolved to"
      , string $ getRealPath path
      ]
    ResolveHeaderNotAttempted header -> hsep [
        "Header"
      , string header.path
      , "not attempted to be resolved"
      ]

instance IsTrace Level ResolveHeaderMsg where
  getDefaultLogLevel = \case
    ResolveHeaderClang x        -> getDefaultLogLevel x
    ResolveHeaderFound{}        -> Info
    ResolveHeaderNotAttempted{} -> Error
  getSource = \case
    ResolveHeaderClang x        -> getSource x
    ResolveHeaderFound{}        -> HsBindgen
    ResolveHeaderNotAttempted{} -> HsBindgen
  getTraceId = const "resolve-header"

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Resolve any number of headers
resolveHeaders ::
     Tracer ResolveHeaderMsg
  -> ClangArgs
  -> Set C.HashIncludeArg
  -> IO (Map C.HashIncludeArg RealPath)
resolveHeaders tracer args headers =
      fmap (fromMaybe Map.empty)
    . withClang' (contramap ResolveHeaderClang tracer) clangSetup
    $ \unit -> do
        root <- clang_getTranslationUnitCursor unit
        successes <- Map.fromList
          <$> HighLevel.clang_visitChildren root visit
        forM_ headerList $ \header -> traceWith tracer $ withCallStack $
          case Map.lookup header successes of
            Just path -> ResolveHeaderFound header path
            Nothing   -> ResolveHeaderNotAttempted header
        return $ Just successes
  where
    headerList :: [C.HashIncludeArg]
    headerList = Set.toAscList headers

    rootHeaderName :: FilePath
    rootHeaderName = "hs-bindgen-resolve.h"

    rootHeaderPath :: SourcePath
    rootHeaderPath = SourcePath $ Text.pack rootHeaderName

    rootHeaderContent :: String
    rootHeaderContent = unlines [
        "#include <" ++ header.path ++ ">"
      | header <- headerList
      ]

    clangSetup :: ClangSetup
    clangSetup = defaultClangSetup args $
      ClangInputMemory rootHeaderName rootHeaderContent

    -- We use the low-level location API here because this fold visits every
    -- cursor, including those in the root header (a virtual in-memory file).
    -- We only need the file name and line number to identify root-header
    -- @#include@ directives, so building a full 'SingleLoc' via
    -- 'clang_getCursorLocation'' would be unnecessary work.
    visit :: Fold IO (C.HashIncludeArg, RealPath)
    visit = simpleFold $ \curr ->
      maybe foldContinue foldContinueWith <=< runMaybeT $ do
        (file, line, _col, _off) <-
          clang_getExpansionLocation =<< clang_getCursorLocation curr
        path <- clang_getFileName file
        guard $ SourcePath path == rootHeaderPath
        guard . (== Right CXCursor_InclusionDirective) . fromSimpleEnum
          =<< clang_getCursorKind curr
        header <- maybe (panicIO "Unknown include") return $
          headerList !? (fromIntegral line - 1)
        rp <- MaybeT
            $ HighLevel.clang_tryGetRealPath =<< clang_getIncludedFile curr
        return (header, rp)
