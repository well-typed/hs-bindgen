module HsBindgen.Frontend.ProcessIncludes (
    GetMainHeadersAndInclude
  , processIncludes
  , GetMainHeaders
  , toGetMainHeaders
    -- * Auxiliary
  , getIncludeTarget
  ) where

import Control.Applicative (asum)
import Data.Digraph qualified as Digraph
import Data.List qualified as List
import Data.List.Compat (unsnoc)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import System.FilePath.Posix qualified as Posix

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.IncludeGraph (Include (..), IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C

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
  command line options, etc.; we use 'C.HashIncludeArg' for this concept.

  == Selecting declarations

  When we see a declaration in the @clang@ AST, we might need to check if this
  declaration is from one of these main headers (as opposed to a header
  /included by/ one of the main headers). Unfortunately, @clang@ does not
  give us a 'C.HashIncludeArg' for the declaration, but rather a 'SourcePath'.
  The exact nature of this 'SourcePath' is a @clang@ internal detail, but it
  might for example be @/the/full/path/to/b.h@.

  In this example, we then /know/ that the set of main headers is @<a.h>@ and
  @<b.h>@, and we need to check if the 'SourcePath' @/the/full/path/to/b.h@
  happens to correspond to one of these main headers. Simply comparing the
  @basename@ is insufficient: it's entirely possible that for example both @b.h@
  and @internal/b.h@ exist in the library (or indeed, this particular @b.h@
  might be from a different library altogether).

  Therefore we need a /mapping/ from 'C.HashIncludeArg' to 'SourcePath', at
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
  if we should use a 'C.HashIncludeArg' or a 'SourcePath' for this purpose;
  we currently choose the former, so that we can generate the somewhat cleaner
  lookling

  > foreign import capi "b.h foo" ..

  rather than

  > foreign import capi "/full/path/to/b.h foo" ..

  Arguably, the lattter is more correct, but the former is perhaps a little more
  portable (although @hs-bindgen@ is in general not intended to produce portable
  code anyway).

  When we see an @#include@ in the root header, we obtain a 'RealPath' for
  the included file (its canonical on-disk path, via 'getIncludeTarget'). To
  determine which 'C.HashIncludeArg' the directive corresponds to, we use the
  directive's /location/ as an index into the root header rather than matching
  on the resolved path.

  (Multiple 'C.HashIncludeArg's in the root header could in principle resolve
  to the /same/ path, so a reverse map would be ambiguous.)
-------------------------------------------------------------------------------}

-- | Function to get the main headers that (transitively) include a source path,
-- as well as the @#include@ argument used to include the source path
type GetMainHeadersAndInclude =
   RealPath -> Either String (NonEmpty C.HashIncludeArg, Include)

-- | Process includes
--
-- We do this as separate pass over the clang AST; this should be relatively
-- cheap, as we can reuse the same 'CXTranslationUnit'.
processIncludes ::
     CXTranslationUnit
  -> IO ( IncludeGraph
        , IsMainHeader
        , IsInMainHeaderDir
        , GetMainHeadersAndInclude
        , [RealPath]
          -- ^ Canonical paths of the main headers (from the root header).
        )
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

    let includeGraphEdges :: IncludeGraph
        includeGraphEdges = IncludeGraph.fromList
          [ (fromRP, incDir.include, incDir.to)
          | incDir <- includes
          , Just fromRP <- [incDir.fromRealPath]
          ]

        mainPathPairs :: [(RealPath, C.HashIncludeArg)]
        mainPathPairs = [
            ( incDir.to
            , IncludeGraph.getIncludeArg incDir.include
            )
          | incDir <- includes
          , Nothing <- [incDir.fromRealPath]
          ]

        mainPathMap :: Map RealPath C.HashIncludeArg
        mainPathMap = Map.fromList mainPathPairs

        mainPaths :: Set RealPath
        mainPaths = Map.keysSet mainPathMap

        includeGraph :: IncludeGraph
        includeGraph = foldl'
          (flip IncludeGraph.insertVertex)
          includeGraphEdges
          (Set.toList mainPaths)

        isMainHeader :: IsMainHeader
        isMainHeader = mkIsMainHeader mainPaths

        isInMainHeaderDir :: IsInMainHeaderDir
        isInMainHeaderDir = mkIsInMainHeaderDir mainPaths

        getMainHeadersAndInclude :: GetMainHeadersAndInclude
        getMainHeadersAndInclude path
          | path `Set.member` mainPaths =
              case Map.lookup path mainPathMap of
                Just arg -> Right (arg NonEmpty.:| [], startInclude)
                Nothing  -> error' "main header not in map"
          | otherwise =
              case reachableMainHeaders of
                []     -> error' "none"
                (x:xs) -> Right (x NonEmpty.:| xs, startInclude)
          where
            error' msg = Left $
              "getMainHeadersAndInclude failed for " ++ show path ++ ": "
                ++ msg

            reachableMainHeaders :: [C.HashIncludeArg]
            reachableMainHeaders =
              mapMaybe (`Map.lookup` mainPathMap)
                . Set.toList
                $ IncludeGraph.reaches includeGraph path

            startInclude :: Include
            startInclude =
              case IncludeGraph.getIncludes includeGraph path of
                Digraph.FindEdgesFound startIncludes _ ->
                  NonEmpty.head startIncludes
                _ -> case Map.lookup path mainPathMap of
                  Just arg -> BracketInclude arg
                  Nothing  -> BracketInclude
                    C.HashIncludeArg{ path = getRealPath path }

    return (
        includeGraph
      , isMainHeader
      , isInMainHeaderDir
      , getMainHeadersAndInclude
      , map fst mainPathPairs
      )

-- | Function to get the main headers that (transitively) include a path
type GetMainHeaders = RealPath -> Either String (NonEmpty C.HashIncludeArg)

toGetMainHeaders :: GetMainHeadersAndInclude -> GetMainHeaders
toGetMainHeaders f = fmap fst . f

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
-- * 'fromRealPath' will be @Just /full/path/to/a.h@ for on-disk files, or
--   'Nothing' for directives in the root header (a virtual in-memory file
--   with no canonical path)
-- * 'include' will be @#include "b.h"@ (the path exactly as written in source)
-- * 'to'      will be @/full/path/to/b.h@ (a 'RealPath' type, the canonical
--   on-disk path, since @#include@ targets are always real files). Using the
--   canonical path also prevents the include graph from having redundant
--   vertices when the same physical file is reached via different paths.
--
-- Both paths depend on how @libclang@ resolves headers, which is affected by
-- @-I@ command line arguments, environment variables such as
-- @C_INCLUDE_PATH@, etc.
data IncDir = IncDir {
      -- | 'Nothing' for @#include@ directives in the root header, which is a
      -- virtual in-memory file with no canonical path on disk.
      fromRealPath :: Maybe RealPath
    , include      :: Include
    , to           :: RealPath
    }

processInclude :: CXTranslationUnit -> CXCursor -> IO IncDir
processInclude unit curr = do
    -- We use the low-level API to get the CXFile directly and try to obtain
    -- its RealPath.  Root-header directives have no real path (the root header
    -- is virtual), so 'clang_tryGetRealPath' returns 'Nothing' for those.
    -- Callers distinguish root-header directives from real-file directives by
    -- checking whether 'fromRealPath' is 'Nothing'.
    (file, _line, _col, _off) <-
      clang_getExpansionLocation =<< clang_getCursorLocation curr
    incDirFromRealPath <- HighLevel.clang_tryGetRealPath file
    incDirTo           <- getIncludeTarget curr
    incDirInclude      <- getInclude unit curr (realPathToSourcePath incDirTo)
    return IncDir{
        fromRealPath = incDirFromRealPath
      , include      = incDirInclude
      , to           = incDirTo
      }

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

getIncludeTarget :: (MonadIO m, HasCallStack) => CXCursor -> m RealPath
getIncludeTarget curr =
    HighLevel.clang_getRealPath =<< clang_getIncludedFile curr

getInclude :: CXTranslationUnit -> CXCursor -> SourcePath -> IO Include
getInclude unit curr path = do
    range  <- toRangeSourcePath =<< clang_getCursorExtent curr
    tokens <- map (\t -> (t.tokenKind, t.tokenSpelling)) <$>
                 HighLevel.clang_tokenize unit getSourcePathText (fmap multiLocExpansion range)
    let err = "Unable to parse #include: " ++ show tokens
    maybe (panicIO err) return $ parseInclude path tokens

type SimpleToken = (SimpleEnum CXTokenKind, TokenSpelling)

parseInclude :: SourcePath -> [SimpleToken] -> Maybe Include
parseInclude path = \case
    t0 : t1 : ts2 -> do
      guard $ isPunctuation t0 && t0 `hasSpelling` "#"
      guard $ isIdentifier t1
      let isIncludeNext = t1 `hasSpelling` "include_next"
      unless isIncludeNext $ guard (t1 `hasSpelling` "include")
      asum [
          parseQuoteIncludeArg   isIncludeNext ts2
        , parseBracketIncludeArg isIncludeNext ts2
        , parseMacroIncludeArg   isIncludeNext ts2
        ]
    _otherwise -> Nothing
  where
    isIdentifier, isLiteral, isPunctuation :: SimpleToken -> Bool
    isIdentifier  = (== Right CXToken_Identifier)  . fromSimpleEnum . fst
    isLiteral     = (== Right CXToken_Literal)     . fromSimpleEnum . fst
    isPunctuation = (== Right CXToken_Punctuation) . fromSimpleEnum . fst

    hasSpelling :: SimpleToken -> Text -> Bool
    hasSpelling = (==) . getTokenSpelling . snd

    parseQuoteIncludeArg :: Bool -> [SimpleToken] -> Maybe Include
    parseQuoteIncludeArg isIncludeNext = \case
      -- Quote include arguments are parsed as literals
      [t] -> do
        guard $ isLiteral t
        let s = Text.unpack $ getTokenSpelling (snd t)
        (cL, s1) <- List.uncons s
        guard $ cL == '"'
        (s', cR) <- unsnoc s1
        guard $ cR == '"'
        let (_, arg) = C.hashIncludeArg s'
        return $
          if isIncludeNext
            then IncludeGraph.QuoteIncludeNext arg
            else IncludeGraph.QuoteInclude     arg
      _otheriwse -> Nothing

    parseBracketIncludeArg :: Bool -> [SimpleToken] -> Maybe Include
    parseBracketIncludeArg isIncludeNext = \case
      -- Bracket include arguments are parsed using punctuation
      t2 : ts3 -> do
        guard $ isPunctuation t2 && t2 `hasSpelling` "<"
        (ts, tR) <- unsnoc ts3
        guard $ isPunctuation tR && tR `hasSpelling` ">"
        -- ts may contain many token kinds, not just identifier/punctuation
        let (_, arg) = C.hashIncludeArg $
              concatMap (Text.unpack . getTokenSpelling . snd) ts
        return $
          if isIncludeNext
            then IncludeGraph.BracketIncludeNext arg
            else IncludeGraph.BracketInclude     arg
      [] -> Nothing

    parseMacroIncludeArg :: Bool -> [SimpleToken] -> Maybe Include
    parseMacroIncludeArg isIncludeNext = \case
      -- Macro include should have at least one argument
      [] -> Nothing
      ts -> do
        let (_, arg) = C.hashIncludeArg $
              Posix.takeFileName (getSourcePath path)
            macroArg = mconcat $ map (getTokenSpelling . snd) ts
        return $
          if isIncludeNext
            then IncludeGraph.MacroIncludeNext arg macroArg
            else IncludeGraph.MacroInclude     arg macroArg
