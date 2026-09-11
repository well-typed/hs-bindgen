module HsBindgen.Resolve (
    -- * Trace messages
    ResolveHeaderMsg(..)
    -- * API
  , resolveHeaders
  ) where

import Control.Monad ((<=<))
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Either (partitionEithers)
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
  | ResolveHeaderNotFound C.HashIncludeArg

    -- | Header not attempted to be resolved, /perhaps/ due to an error while
    -- parsing previous headers
    --
    -- NOTE: We have not been able to construct an example that causes this to
    -- happen, but we can at least detect if it happens.
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
    ResolveHeaderNotFound header -> hsep [
        "Header"
      , string header.path
      , "could not be resolved (header not found)"
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
    ResolveHeaderNotFound{}     -> Error
    ResolveHeaderNotAttempted{} -> Error
  getSource = \case
    ResolveHeaderClang x        -> getSource x
    ResolveHeaderFound{}        -> HsBindgen
    ResolveHeaderNotFound{}     -> HsBindgen
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
        (notFounds, successes) <-
          bimap Set.fromList Map.fromList . partitionEithers
            <$> HighLevel.clang_visitChildren root visit
        forM_ headerList $ \header -> traceWith tracer $ withCallStack $
          case Map.lookup header successes of
            Just path -> ResolveHeaderFound header path
            Nothing
              | Set.member header notFounds -> ResolveHeaderNotFound header
              | otherwise -> ResolveHeaderNotAttempted header
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
    visit :: Fold IO (Either C.HashIncludeArg (C.HashIncludeArg, RealPath))
    visit = simpleFold $ \curr ->
      maybe foldContinue foldContinueWith <=< runMaybeT $ do
        (file, line, _col, _off) <-
          clang_getExpansionLocation =<< clang_getCursorLocation curr
        path <- clang_getFileName file
        guard $ SourcePath path == rootHeaderPath
        -- Only process inclusion directives
        guard . (== Right CXCursor_InclusionDirective) . fromSimpleEnum
          =<< clang_getCursorKind curr
        -- Process inclusion directive
        header <- maybe (panicIO "Unknown include") return $
          headerList !? (fromIntegral line - 1)
        includedFile <- clang_getIncludedFile curr
        mRealPath <- liftIO $ HighLevel.clang_tryGetRealPath includedFile
        return $ case mRealPath of
          Nothing -> Left header
          Just rp -> Right (header, rp)
