module HsBindgen.Resolve (
    -- * Trace messages
    ResolveHeaderMsg(..)
    -- * API
  , resolveHeaders
  ) where

import Control.Monad ((<=<))
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

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
import HsBindgen.Util.List ((!?))
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint (hang, hsep, string)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data ResolveHeaderMsg =
    ResolveHeaderClang ClangMsg
  | ResolveHeaderSuccess  HashIncludeArg SourcePath
  | ResolveHeaderNotFound HashIncludeArg
  deriving stock (Eq, Show)

instance PrettyForTrace ResolveHeaderMsg where
  prettyForTrace = \case
    ResolveHeaderClang msg -> hang
      "during header resolution:" 2 (prettyForTrace msg)
    ResolveHeaderSuccess header path -> hsep [
        "header"
      , string $ getHashIncludeArg header
      , "resolved to"
      , string $ getSourcePath path
      ]
    ResolveHeaderNotFound header -> hsep [
        "header"
      , string $ getHashIncludeArg header
      , "not found"
      ]

instance HasDefaultLogLevel ResolveHeaderMsg where
  getDefaultLogLevel = \case
    ResolveHeaderClang x    -> getDefaultLogLevel x
    ResolveHeaderSuccess{}  -> Info
    ResolveHeaderNotFound{} -> Error

instance HasSource ResolveHeaderMsg where
  getSource = \case
    ResolveHeaderClang x    -> getSource x
    ResolveHeaderSuccess{}  -> HsBindgen
    ResolveHeaderNotFound{} -> HsBindgen

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Resolve any number of headers
resolveHeaders ::
     Tracer IO ResolveHeaderMsg
  -> ClangArgs
  -> Set HashIncludeArg
  -> IO (Map HashIncludeArg SourcePath)
resolveHeaders tracer args headers =
      fmap (fromMaybe Map.empty)
    . withClang' (contramap ResolveHeaderClang tracer) clangSetup
    $ \unit -> do
        root     <- clang_getTranslationUnitCursor unit
        includes <- Map.fromList <$> HighLevel.clang_visitChildren root visit
        forM_ headerList $ \header ->
            traceWith tracer
          . maybe (ResolveHeaderNotFound header) (ResolveHeaderSuccess header)
          $ Map.lookup header includes
        return $ Just includes
  where
    headerList :: [HashIncludeArg]
    headerList = Set.toAscList headers

    rootHeaderName :: FilePath
    rootHeaderName = "hs-bindgen-resolve.h"

    rootHeaderPath :: SourcePath
    rootHeaderPath = SourcePath $ Text.pack rootHeaderName

    rootHeaderContent :: String
    rootHeaderContent = unlines [
        "#include <" ++ getHashIncludeArg header ++ ">"
      | header <- headerList
      ]

    clangSetup :: ClangSetup
    clangSetup = defaultClangSetup args $
      ClangInputMemory rootHeaderName rootHeaderContent

    visit :: Fold IO (HashIncludeArg, SourcePath)
    visit = simpleFold $ \curr ->
      maybe foldContinue foldContinueWith <=< runMaybeT $ do
        loc  <- clang_getCursorLocation curr
        sloc <- HighLevel.clang_getExpansionLocation loc
        let srcPath = singleLocPath sloc
        -- Skip builtin macros
        guard $ not (nullSourcePath srcPath)
        -- Only process the root header
        guard $ srcPath == rootHeaderPath
        -- Only process inclusion directives
        guard . (== Right CXCursor_InclusionDirective) . fromSimpleEnum
          =<< clang_getCursorKind curr
        -- Only process when resolved
        path <- clang_getFileName =<< clang_getIncludedFile curr
        guard $ not (Text.null path)
        -- Process include
        header <- maybe (panicIO "resolveHeaders unknown include") return $
          headerList !? (singleLocLine sloc - 1)
        return (header, SourcePath path)
