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
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Clang
import HsBindgen.Errors (panicIO)
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data ResolveHeaderMsg =
    ResolveHeaderClang ClangMsg
  | ResolveHeaderFound HashIncludeArg SourcePath
  | ResolveHeaderNotFound HashIncludeArg

    -- | Header not attempted to be resolved, /perhaps/ due to an error while
    -- parsing previous headers
    --
    -- NOTE: We have not been able to construct an example that causes this to
    -- happen, but we can at least detect if it happens.
  | ResolveHeaderNotAttempted HashIncludeArg
  deriving stock (Show)

instance PrettyForTrace ResolveHeaderMsg where
  prettyForTrace = \case
    ResolveHeaderClang msg -> hang
      "During header resolution:" 2 (prettyForTrace msg)
    ResolveHeaderFound header path -> hsep [
        "Header"
      , string header.path
      , "resolved to"
      , string $ getSourcePath path
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
  -> Set HashIncludeArg
  -> IO (Map HashIncludeArg SourcePath)
resolveHeaders tracer args headers =
      fmap (fromMaybe Map.empty)
    . withClang' (contramap ResolveHeaderClang tracer) clangSetup
    $ \unit -> do
        root <- clang_getTranslationUnitCursor unit
        (notFounds, successes) <-
          bimap Set.fromList Map.fromList . partitionEithers
            <$> HighLevel.clang_visitChildren root visit
        forM_ headerList $ \header -> traceWith tracer $
          case Map.lookup header successes of
            Just path -> ResolveHeaderFound header path
            Nothing
              | Set.member header notFounds -> ResolveHeaderNotFound header
              | otherwise -> ResolveHeaderNotAttempted header
        return $ Just successes
  where
    headerList :: [HashIncludeArg]
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

    visit :: Fold IO (Either HashIncludeArg (HashIncludeArg, SourcePath))
    visit = simpleFold $ \curr ->
      maybe foldContinue foldContinueWith <=< runMaybeT $ do
        sloc <- HighLevel.clang_getCursorLocation' curr
        -- Only process the root header
        guard $ singleLocPath sloc == rootHeaderPath
        -- Only process inclusion directives
        guard . (== Right CXCursor_InclusionDirective) . fromSimpleEnum
          =<< clang_getCursorKind curr
        -- Process inclusion directive
        header <- maybe (panicIO "Unknown include") return $
          headerList !? (singleLocLine sloc - 1)
        path <- clang_getFileName =<< clang_getIncludedFile curr
        return $
          if Text.null path
            then Left header
            else Right (header, SourcePath path)
