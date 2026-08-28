module HsBindgen.Frontend.ProcessIncludes (
    GetMainHeadersAndInclude
  , processIncludes
    -- * Auxiliary
  , getIncludeTo
  , namesFile
  ) where

import Control.Applicative (asum)
import Data.Digraph qualified as Digraph
import Data.List qualified as List
import Data.List.Compat (unsnoc)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import System.FilePath qualified as FilePath
import System.FilePath.Posix qualified as Posix

import Clang.Args
import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Clang (ClangMsg)
import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.HeaderName (ProjectRoot)
import HsBindgen.Frontend.Analysis.HeaderName qualified as HeaderName
import HsBindgen.Frontend.Analysis.IncludeGraph (Include, IncludeGraph,
                                                 SourceFile (..))
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Imports
import HsBindgen.IR.C (FileId, HeaderName)
import HsBindgen.IR.C qualified as C
import HsBindgen.Util.Tracer

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

  When we see the @#include@ in the root header, @clang@ again only gives us
  a 'SourcePath' for the file-to-be-included. We ignore this, and instead use
  its /location/ as an index into the root header.

  (Note that we cannot really build a map from 'SourcePath' to
  'C.HashIncludeArg': multiple 'C.HashIncludeArg's in the root header could
  in principle resolve to the /same/ 'SourcePath.')
-------------------------------------------------------------------------------}

-- | Function to get the main headers that (transitively) include a source file,
-- as well as the @#include@ argument used to include it
type GetMainHeadersAndInclude =
   SourceFile
     -> Either String (NonEmpty C.HashIncludeArg, Include, IncludeGraph.Header)

-- | Process includes
--
-- We do this as separate pass over the clang AST; this should be relatively
-- cheap, as we can reuse the same 'CXTranslationUnit'.
--
-- Naming the files costs one extra parse, of a synthetic header holding every
-- candidate name at once. See 'HeaderName.headerNamesOf'.
processIncludes ::
     Tracer ClangMsg
  -> ClangArgs      -- ^ the arguments this unit was parsed with
  -> ProjectRoot
  -> [CIncludeDir]  -- ^ include search path, in order
  -> CXTranslationUnit
  -> IO ( IncludeGraph
        , GetMainHeadersAndInclude
        , [SourcePath]
          -- ^ Resolved paths of the main headers (from the root header).
          -- These are the actual filesystem paths that clang resolved from
          -- the user's @#include@ arguments.
        )
processIncludes tracer args projectRoot incDirs unit = do
    root     <- clang_getTranslationUnitCursor unit
    includes <- HighLevel.clang_visitChildren root $ simpleFold $ \curr -> do
                  mKind <- fromSimpleEnum <$> clang_getCursorKind curr
                  case mKind of
                    Right CXCursor_InclusionDirective -> do
                      include <- processInclude unit curr
                      foldContinueWith include
                    _otherwise ->
                      foldContinue

    -- Every file we saw, named in one batch.
    let allFiles :: [FileId]
        allFiles = Set.toList . Set.fromList $
          [ fileId
          | incDir <- includes
          , OnDisk fileId <- [incDir.from, incDir.to]
          ]
    names <- HeaderName.headerNamesOf tracer args projectRoot incDirs allFiles

    let headers :: Map FileId IncludeGraph.Header
        headers = Map.mapWithKey mkHeader names

        mkHeader :: FileId -> HeaderName -> IncludeGraph.Header
        mkHeader fileId name = IncludeGraph.Header{
              name    = name
            , aliases = Map.findWithDefault Set.empty fileId aliasMap
            }

        -- An include argument is an alias when it does not name the file it
        -- reached, which is exactly when showing the derived name would put a
        -- different name in front of the reader than the one they wrote.
        --
        -- Compared against the argument alone, segment by segment. Comparing
        -- against the path clang reported would need it made absolute first,
        -- and would then call every header an alias of itself whenever @-I@ was
        -- given a relative directory.
        aliasMap :: Map FileId (Set C.HashIncludeArg)
        aliasMap = Map.fromListWith Set.union [
            (fileId, Set.singleton arg)
          | incDir <- includes
          , OnDisk fileId <- [incDir.to]
          , let arg = IncludeGraph.getIncludeArg incDir.include
          , not (namesFile fileId arg)
          ]

        -- Every name clang printed for a file, pointing at the file. Both
        -- endpoints contribute: the including file is reported by its first
        -- lookup name, the included file by the spelling that requested it.
        pathIndex :: Map SourcePath SourceFile
        pathIndex = Map.fromList $ concat [
            [ (incDir.fromReported, incDir.from)
            , (incDir.toReported,   incDir.to)
            ]
          | incDir <- includes
          ]

        includeGraph :: IncludeGraph
        includeGraph =
          IncludeGraph.fromEdges
            [ (incDir.from, incDir.include, incDir.to) | incDir <- includes ]
            headers
            pathIndex
            mainNames

        mainFilePairs :: [(SourceFile, C.HashIncludeArg)]
        mainFilePairs = [
            (incDir.to, IncludeGraph.getIncludeArg incDir.include)
          | incDir <- includes
          , incDir.inRoot
          ]

        -- Taken from the header map rather than from the graph, which is
        -- already carrying this set by the time it is built.
        mainNames :: Set HeaderName
        mainNames = Set.fromList [
            header.name
          | (OnDisk fileId, _arg) <- mainFilePairs
          , Just header           <- [Map.lookup fileId headers]
          ]

        getMainHeadersAndInclude :: GetMainHeadersAndInclude
        getMainHeadersAndInclude file =
          let error' msg = Left $
                "getMainHeadersAndInclude failed for " ++ show file ++ ": "
                  ++ msg
          in  case IncludeGraph.lookupHeader includeGraph file of
                Nothing     -> error' "no header name"
                Just header -> case IncludeGraph.getIncludes includeGraph file of
                  Digraph.FindEdgesFound startIncludes termIncludes -> Right $
                    ( IncludeGraph.getIncludeArg <$> termIncludes
                    , NonEmpty.head startIncludes
                    , header
                    )
                  Digraph.FindEdgesNone    -> error' "none"
                  Digraph.FindEdgesInvalid -> error' "invalid"

    return (
        includeGraph
      , getMainHeadersAndInclude
      , [ incDir.toReported | incDir <- includes, incDir.inRoot ]
      )

-- | Does this @#include@ argument name the file it reached?
--
-- True when the file's real path ends in whatever the argument works out to,
-- so @\<widget\/core.h\>@ names @\/abs\/include\/widget\/core.h@, and so do
-- @\"..\/core.h\"@ and the roundabout @\<widget\/..\/widget\/core.h\>@. False
-- when it works out to something else, as @\<widget\/alias.h\>@ does for a file
-- called @core.h@.
--
-- Lexical on purpose: this asks what the argument calls the file, not how the
-- filesystem reached it, and the two come apart in both directions. A symlink
-- whose name matches its target's is not reported, because nothing was renamed
-- and the reader is shown the name they wrote. A search directory that is
-- itself a symlink is not reported either, which is the point: resolving the
-- argument on disk would make every header under it an alias.
namesFile :: FileId -> C.HashIncludeArg -> Bool
namesFile fileId arg = argSegs `List.isSuffixOf` fileSegs
  where
    -- Split each side the way it was written. A real path came from the
    -- filesystem and uses this platform's separator; an @#include@ argument is
    -- C syntax and uses forward slashes wherever it runs. Splitting the real
    -- path as POSIX would leave a Windows path in one piece, and then nothing
    -- would ever match.
    fileSegs, argSegs :: [FilePath]
    fileSegs = FilePath.splitDirectories fileId.path
    argSegs  = dropWhile (== "..") . resolveDotDot $
                 Posix.splitDirectories arg.path

-- | Cancel each @..@ against the segment before it, keeping the ones that escape
--
-- A @..@ cancels what precedes it, so an argument has to be worked out rather
-- than merely stripped of dots: dropping both halves of @widget\/..@ would
-- leave @widget\/widget\/core.h@ and make the file an alias of itself.
--
-- What survives is only ever leading, since a @..@ is kept only when there is
-- nothing left to cancel against. 'namesFile' drops those: they walk above
-- where the argument starts, which says nothing about what the file is called.
resolveDotDot :: [FilePath] -> [FilePath]
resolveDotDot = reverse . List.foldl' step []
  where
    -- Accumulated in reverse, so cancelling is a look at the head.
    step :: [FilePath] -> FilePath -> [FilePath]
    step acc = \case
      "."  -> acc
      ".." -> case acc of
                prev : rest | prev /= ".." -> rest
                _otherwise                 -> ".." : acc
      seg  -> seg : acc

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
-- * 'from'       identifies @a.h@
-- * 'include'    will be @#include "b.h"@ (exact path as in source)
-- * 'to'         identifies @b.h@
-- * 'toReported' is the path @clang@ reported for @b.h@, which is the
--   directive spelling resolved against the search path, not an identity
-- * 'inRoot'     will be 'True' if the include is in the root header
--
-- 'from' and 'to' are real paths, so one file is one 'SourceFile' whichever
-- spelling reached it. 'toReported' is kept only to tell a symlinked route
-- apart from a roundabout spelling of the same path.
data IncDir = IncDir {
      from         :: SourceFile
    , fromReported :: SourcePath
    , include      :: Include
    , to           :: SourceFile
    , toReported   :: SourcePath
    , inRoot       :: Bool
    }

processInclude :: CXTranslationUnit -> CXCursor -> IO IncDir
processInclude unit curr = do
    incDirFromLoc <- HighLevel.clang_getCursorLocation' curr
    incDirFrom    <- sourceFileOfLoc curr (singleLocPath incDirFromLoc)
    includedFile  <- clang_getIncludedFile curr
    incDirTo      <- sourceFileOf includedFile
    reported      <- SourcePath <$> clang_getFileName includedFile
    incDirInclude <- getInclude unit curr reported
    incDirInRoot  <-
      clang_Location_isFromMainFile =<< clang_getCursorLocation curr
    return IncDir{
        from         = incDirFrom
      , fromReported = singleLocPath incDirFromLoc
      , include      = incDirInclude
      , to           = incDirTo
      , toReported   = reported
      , inRoot       = incDirInRoot
      }

-- | Identify the file a cursor sits in
--
-- Falls back to the reported path when @clang@ has no real path, which is the
-- case for the synthetic root header.
sourceFileOfLoc :: CXCursor -> SourcePath -> IO SourceFile
sourceFileOfLoc curr reported = do
    (file, _line, _column, _offset) <-
      clang_getFileLocation =<< clang_getCursorLocation curr
    maybe (InMemory reported) OnDisk <$> HeaderName.fileIdOf file

sourceFileOf :: CXFile -> IO SourceFile
sourceFileOf file = do
    mFileId <- HeaderName.fileIdOf file
    case mFileId of
      Just fileId -> return (OnDisk fileId)
      Nothing     -> InMemory . SourcePath <$> clang_getFileName file

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

getIncludeTo :: MonadIO m => CXCursor -> m SourcePath
getIncludeTo curr = do
    file <- clang_getIncludedFile curr
    SourcePath <$> clang_getFileName file

getInclude :: CXTranslationUnit -> CXCursor -> SourcePath -> IO Include
getInclude unit curr path = do
    tokens <- HighLevel.clang_tokenize unit . fmap multiLocExpansion
      =<< HighLevel.clang_getCursorExtent curr
    let err = "Unable to parse #include: " ++ show tokens
    maybe (panicIO err) return $ parseInclude path tokens

parseInclude :: SourcePath -> [Token TokenSpelling] -> Maybe Include
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
    isIdentifier, isLiteral, isPunctuation :: Token a -> Bool
    isIdentifier  = (== Right CXToken_Identifier)  . fromSimpleEnum . tokenKind
    isLiteral     = (== Right CXToken_Literal)     . fromSimpleEnum . tokenKind
    isPunctuation = (== Right CXToken_Punctuation) . fromSimpleEnum . tokenKind

    hasSpelling :: Token TokenSpelling -> Text -> Bool
    hasSpelling = (==) . (getTokenSpelling . tokenSpelling)

    parseQuoteIncludeArg :: Bool -> [Token TokenSpelling] -> Maybe Include
    parseQuoteIncludeArg isIncludeNext = \case
      -- Quote include arguments are parsed as literals
      [t] -> do
        guard $ isLiteral t
        let s = Text.unpack $ getTokenSpelling (tokenSpelling t)
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

    parseBracketIncludeArg :: Bool -> [Token TokenSpelling] -> Maybe Include
    parseBracketIncludeArg isIncludeNext = \case
      -- Bracket include arguments are parsed using punctuation
      t2 : ts3 -> do
        guard $ isPunctuation t2 && t2 `hasSpelling` "<"
        (ts, tR) <- unsnoc ts3
        guard $ isPunctuation tR && tR `hasSpelling` ">"
        -- ts may contain many token kinds, not just identifier/punctuation
        let (_, arg) = C.hashIncludeArg $
              concatMap (Text.unpack . getTokenSpelling . tokenSpelling) ts
        return $
          if isIncludeNext
            then IncludeGraph.BracketIncludeNext arg
            else IncludeGraph.BracketInclude     arg
      [] -> Nothing

    parseMacroIncludeArg :: Bool -> [Token TokenSpelling] -> Maybe Include
    parseMacroIncludeArg isIncludeNext = \case
      -- Macro include should have at least one argument
      [] -> Nothing
      ts -> do
        let (_, arg) = C.hashIncludeArg $
              Posix.takeFileName (getSourcePath path)
            macroArg = mconcat $ map (getTokenSpelling . tokenSpelling) ts
        return $
          if isIncludeNext
            then IncludeGraph.MacroIncludeNext arg macroArg
            else IncludeGraph.MacroInclude     arg macroArg
