module HsBindgen.Frontend.ProcessIncludes (
    GetMainHeadersAndInclude
  , processIncludes
    -- * Auxiliary
  , getIncludeTo
  ) where

import Data.DynGraph.Labelled qualified as DynGraph
import Data.List qualified as List
import Data.List.Compat (unsnoc)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Clang.Enum.Simple

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types

import Clang.LowLevel.Core

import Clang.Paths

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.IncludeGraph (Include, IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.RootHeader (HashIncludeArg)
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
  > #include <b.h>

  These paths must be interpreted with respect to the @C_INCLUDE_PATH@, the @-I@
  command line options, etc.; we use 'HashIncludeArg' for this concept.

  == Selecting declarations

  When we see a declaration in the @clang@ AST, we might need to check if this
  declaration is from one of these main headers (as opposed to a header
  /included by/ one of the main headers). Unfortunately, @clang@ does not
  give us a 'HashIncludeArg' for the declaration, but rather a 'SourcePath'.
  The exact nature of this 'SourcePath' is a @clang@ internal detail, but it
  might for example be @/the/full/path/to/b.h@.

  In this example, we then /know/ that the set of main headers is @<a.h>@ and
  @<b.h>@, and we need to check if the 'SourcePath' @/the/full/path/to/b.h@
  happens to correspond to one of these main headers. Simply comparing the
  @basename@ is insufficient: it's entirely possible that for example both @b.h@
  and @internal/b.h@ exist in the library (or indeed, this particular @b.h@
  might be from a different library altogether).

  Therefore we need a /mapping/ from 'HashIncludeArg' to 'SourcePath', at
  least for the includes in the root header. The only reliable way that we found
  to get this mapping is by looking at how @clang@ resolves these headers as it
  parses the root header (there is an API specifically for resolving header
  paths, but it does subtly different things than the @clang@ parser does).

  Unfortunately, this means that we need to process all includes /prior/ to
  processing the rest of the AST, because we are not guaranteed to see the
  include of @b.h@ from the root header prior to processing it: this will
  /usually/ be the case, but not if @<a.h>@ /itself/ also includes @<b.h>@.

  == Setting the current main header

  When we see a function declaration, we must associate that function
  declaration with one of the main headers (so that we can generate the correct
  @#include@ when producing code for that function). It's not entirely obvious
  if we should use a 'HashIncludeArg' or a 'SourcePath' for this purpose;
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
  'HashIncludeArg': multiple 'HashIncludeArg's in the root header could
  in principle resolve to the /same/ 'SourcePath.')
-------------------------------------------------------------------------------}

-- | Function to get the main headers that (transitively) include a source path,
-- as well as the @#include@ argument used to include the source path
type GetMainHeadersAndInclude =
   SourcePath -> Either String (NonEmpty HashIncludeArg, HashIncludeArg)

-- | Process includes
--
-- We do this as separate pass over the clang AST; this should be relatively
-- cheap, as we can reuse the same 'CXTranslationUnit'.
processIncludes ::
     CXTranslationUnit
  -> IO (IncludeGraph, IsMainHeader, IsInMainHeaderDir, GetMainHeadersAndInclude)
processIncludes unit = do
    root     <- clang_getTranslationUnitCursor unit
    includes <- HighLevel.clang_visitChildren root $ simpleFold $ \curr -> do
                  mKind <- fromSimpleEnum <$> clang_getCursorKind curr
                  case mKind of
                    Right CXCursor_InclusionDirective -> do
                      include <- processInclude unit curr
                      foldContinueWith include
                    _otherwise ->
                      foldContinue

    let includeGraph :: IncludeGraph
        includeGraph = IncludeGraph.fromList $
          map (\IncDir{..} -> (incDirFrom, incDirInclude, incDirTo)) includes

        mainPathPairs :: [(SourcePath, HashIncludeArg)]
        mainPathPairs = [
            (incDirTo, IncludeGraph.includeArg incDirInclude)
          | IncDir{..} <- includes
          , incDirInRoot
          ]

        mainPathMap :: Map SourcePath HashIncludeArg
        mainPathMap = Map.fromList mainPathPairs

        lookupMainPath :: SourcePath -> Either String HashIncludeArg
        lookupMainPath path = case Map.lookup path mainPathMap of
          Just header -> Right header
          Nothing     -> Left $ "main path not found: " ++ show path

        mainPaths :: Set SourcePath
        mainPaths = Map.keysSet mainPathMap

        isMainHeader :: IsMainHeader
        isMainHeader = mkIsMainHeader mainPaths

        isInMainHeaderDir :: IsInMainHeaderDir
        isInMainHeaderDir = mkIsInMainHeaderDir mainPaths

        getMainHeadersAndInclude :: GetMainHeadersAndInclude
        getMainHeadersAndInclude path =
          let error' msg = Left $
                "getMainHeadersAndInclude failed for " ++ show path ++ ": "
                  ++ msg
          in  case IncludeGraph.getIncludes mainPaths includeGraph path of
                DynGraph.FindTargetsFound headers include -> do
                  headers' <- mapM lookupMainPath headers
                  return (headers', IncludeGraph.includeArg include)
                DynGraph.FindTargetsTarget -> do
                  header <- lookupMainPath path
                  return (NonEmpty.singleton header, header)
                DynGraph.FindTargetsNotFound -> error' "not found"
                DynGraph.FindTargetsInvalid  -> error' "invalid"

    return (
        includeGraph
      , isMainHeader
      , isInMainHeaderDir
      , getMainHeadersAndInclude
      )

{-------------------------------------------------------------------------------
  Process inclusion directives
-------------------------------------------------------------------------------}

-- | Include directive
--
-- Suppose we have file @a.h@ containing
--
-- > #include "b.h"
--
-- Then
--
-- * 'incDirFrom'    will be @/full/path/to/a.h@
-- * 'incDirInclude' will be @#include "b.h"@ (exact path as in source)
-- * 'incDirTo'      will be @/full/path/to/b.h@
-- * 'incDirInRoot'  will be 'True' if the include is in the root header
--
-- The full 'SourcePath's are constructed by @libclang@, and depend on factors
-- such as @-I@ command line arguments, environment variables such as
-- @C_INCLUDE_PATH@, etc.
data IncDir = IncDir {
      incDirFrom    :: SourcePath
    , incDirInclude :: Include
    , incDirTo      :: SourcePath
    , incDirInRoot  :: Bool
    }

processInclude :: CXTranslationUnit -> CXCursor -> IO IncDir
processInclude unit curr = do
    incDirFrom    <- getIncludeFrom curr
    incDirInclude <- getInclude unit curr
    incDirTo      <- getIncludeTo curr
    incDirInRoot  <-
      clang_Location_isFromMainFile =<< clang_getCursorLocation curr
    return IncDir{..}

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

getInclude :: CXTranslationUnit -> CXCursor -> IO Include
getInclude unit curr = do
    tokens <- HighLevel.clang_tokenize unit . fmap multiLocExpansion
      =<< HighLevel.clang_getCursorExtent curr
    let err = "unable to parse #include: " ++ show tokens
    maybe (panicIO err) return $ parseInclude tokens

parseInclude :: [Token TokenSpelling] -> Maybe Include
parseInclude = \case
    t0 : t1 : ts2 -> do
      guard $ isPunctuation t0 && t0 `hasSpelling` "#"
      guard $ isIdentifier t1
      let isIncludeNext = t1 `hasSpelling` "include_next"
      unless isIncludeNext $ guard (t1 `hasSpelling` "include")
      case ts2 of
        -- quote include arguments are parsed as literals
        [t] -> do
          guard $ isLiteral t
          let s = Text.unpack $ getTokenSpelling (tokenSpelling t)
          (cL, s1) <- List.uncons s
          guard $ cL == '"'
          (s', cR) <- unsnoc s1
          guard $ cR == '"'
          let (_, arg) = RootHeader.hashIncludeArg s'
          return $
            if isIncludeNext
              then IncludeGraph.QuoteIncludeNext arg
              else IncludeGraph.QuoteInclude arg
        -- bracket include arguments are parsed using punctuation
        t2 : ts3 -> do
          guard $ isPunctuation t2 && t2 `hasSpelling` "<"
          (ts, tR) <- unsnoc ts3
          guard $ isPunctuation tR && tR `hasSpelling` ">"
          -- ts may contain many token kinds, not just identifier/punctuation
          let (_, arg) = RootHeader.hashIncludeArg $
                concatMap (Text.unpack . getTokenSpelling . tokenSpelling) ts
          return $
            if isIncludeNext
              then IncludeGraph.BracketIncludeNext arg
              else IncludeGraph.BracketInclude arg
        [] -> Nothing
    _otherwise -> Nothing
  where
    isIdentifier, isLiteral, isPunctuation :: Token a -> Bool
    isIdentifier  = (== Right CXToken_Identifier)  . fromSimpleEnum . tokenKind
    isLiteral     = (== Right CXToken_Literal)     . fromSimpleEnum . tokenKind
    isPunctuation = (== Right CXToken_Punctuation) . fromSimpleEnum . tokenKind

    hasSpelling :: Token TokenSpelling -> Text -> Bool
    hasSpelling = (==) . (getTokenSpelling . tokenSpelling)
