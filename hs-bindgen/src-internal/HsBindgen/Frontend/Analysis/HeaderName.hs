-- | Deriving portable header names
--
-- The types themselves live in "HsBindgen.IR.C.HeaderName"; this module is the
-- part that needs @clang@.
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.HeaderName qualified as HeaderName
module HsBindgen.Frontend.Analysis.HeaderName (
    -- * File identity
    fileIdOf
    -- * Trace messages
  , HeaderNameMsg(..)
    -- * Derivation
  , ProjectRoot(..)
  , getProjectRoot
  , headerNamesOf
  , resolveHeaderNames
  , includeSearchPath
  ) where

import Data.Containers.ListUtils (nubOrd)
import Data.List qualified as List
import Data.Maybe (listToMaybe)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import System.Directory qualified as Dir
import System.FilePath qualified as FilePath
import Text.SimplePrettyPrint qualified as PP

import Clang.Args
import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Clang
import HsBindgen.Imports
import HsBindgen.IR.C (FileId, HeaderName (..))
import HsBindgen.IR.C qualified as C
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  File identity
-------------------------------------------------------------------------------}

-- | The 'FileId' of a @clang@ file handle
--
-- Returns 'Nothing' for files that have no real path, which is the case for the
-- synthetic root header and any other unsaved input.
fileIdOf :: MonadIO m => CXFile -> m (Maybe FileId)
fileIdOf file =
    fmap (C.FileId . Text.unpack) <$> clang_File_tryGetRealPathName file

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

-- | Messages emitted while naming headers
data HeaderNameMsg =
    HeaderNameClang ClangMsg

    -- | A header has no bracket name, because another file answers to one of
    -- the names it could have used
    --
    -- Reported because within one project this usually means an earlier @-I@
    -- is hiding a header behind another, which is not something anyone sets
    -- out to do. It is not an error and not always a mistake: the C standard
    -- library shadows itself, since @clang@ ships headers that reach the
    -- platform's through @#include_next@.
  | HeaderShadowed
      C.HashIncludeArg  -- ^ the name that was taken
      FileId            -- ^ the file that answers to it
      HeaderName        -- ^ what we called this header instead
  deriving stock (Show)

instance PrettyForTrace HeaderNameMsg where
  prettyForTrace = \case
    HeaderNameClang msg -> prettyForTrace msg
    HeaderShadowed arg by name -> PP.string $ concat [
        "#include <", arg.path, "> reaches ", by.path
      , ", so this header is named: ", C.renderHeaderName name
      ]

instance IsTrace Level HeaderNameMsg where
  getDefaultLogLevel = \case
    HeaderNameClang msg -> getDefaultLogLevel msg
    -- Fires on the C standard library on an ordinary system, so it belongs
    -- with the rest of the "why did that come out like that" detail rather
    -- than among the warnings.
    HeaderShadowed{}    -> Info
  getSource = \case
    HeaderNameClang msg -> getSource msg
    HeaderShadowed{}    -> HsBindgen
  getTraceId = \case
    HeaderNameClang msg -> getTraceId msg
    HeaderShadowed{}    -> "header-shadowed"

{-------------------------------------------------------------------------------
  The search path
-------------------------------------------------------------------------------}

-- | The include search path, read back off the arguments @clang@ was given
--
-- Read from the arguments rather than the configuration, since a @-I@ can
-- arrive through @argsBefore@, @argsInner@, @argsAfter@ or
-- @BINDGEN_EXTRA_CLANG_ARGS@ as easily as through @extraIncludeDirs@, and the
-- builtin directory arrives as @-isystem@.
--
-- Only the order of preference depends on this being complete. 'candidatesFor'
-- also offers the suffixes of a file's own path, so a directory missed here
-- costs a better name rather than any name at all.
--
-- Ordered as @clang@ searches: quote directories, then @-I@, then system
-- directories. Both the joined (@-Ifoo@) and separated (@-I foo@) spellings
-- are read. Case is what tells @-I@ apart from @-isystem@ and its relatives,
-- which all start with a lowercase @i@.
includeSearchPath :: ClangArgs -> [CIncludeDir]
includeSearchPath (ClangArgs args) =
    map CIncludeDir $ concat [dirs "-iquote", dirs "-I", dirs "-isystem"]
  where
    dirs :: String -> [FilePath]
    dirs flag = go args
      where
        go = \case
          f : dir : rest | f == flag           -> dir : go rest
          a : rest
            | Just d <- List.stripPrefix flag a
            , not (null d)                     -> d : go rest
          _ : rest                             -> go rest
          []                                   -> []

{-------------------------------------------------------------------------------
  Derivation
-------------------------------------------------------------------------------}

-- | Directory that quote names are relative to
--
-- Quote includes resolve against the directory of the file containing the
-- directive. We give the synthetic root header an absolute name inside this
-- directory, which pins the anchor here rather than leaving it to wherever the
-- process happens to have been started.
newtype ProjectRoot = ProjectRoot { path :: FilePath }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- | The working directory, canonicalised, as the project root
--
-- This matches how a relative @-I@ flag is already read (see t'CIncludeDir'),
-- so quote names and the search path they must agree with share one anchor.
getProjectRoot :: IO ProjectRoot
getProjectRoot = ProjectRoot <$> (Dir.canonicalizePath =<< Dir.getCurrentDirectory)

-- | Name every given file
--
-- A name is an @#include@ argument that resolves back to the file it names, so
-- every candidate is offered to @clang@ and kept only if it comes back with
-- the file we started from. Rejecting the rest is what stops a shadowed header
-- being keyed under the shadowing one's name, which would bind it with the
-- wrong layout.
--
-- Candidates come from 'candidatesFor', best first, and the first that
-- resolves wins. Do not shortcut that by picking a directory up front: it
-- yields 'ByQuote' where a later candidate would have given a bracket name. A
-- file no bracket name reaches falls back to 'ByQuote' from the project root,
-- which always resolves, so every file gets a name.
--
-- What a file is called depends on the file and the search path, and on
-- nothing about the directive that happened to reach it. That is what makes a
-- name survive a change of include order, and it is why the search path has to
-- be the one the main parse used.
headerNamesOf ::
     Tracer HeaderNameMsg
  -> ClangArgs      -- ^ must match the main parse, or answers diverge
  -> ProjectRoot    -- ^ anchor for quote names
  -> [CIncludeDir]  -- ^ search path, in order
  -> [FileId]       -- ^ every file to name
  -> IO (Map FileId HeaderName)
headerNamesOf tracer args root incDirs files = do
    dirs <- mapM canonicalDir incDirs
    let candidates :: [(FileId, [C.HashIncludeArg])]
        candidates = [ (file, candidatesFor dirs file) | file <- files ]

    -- Shadowing makes repeated arguments the normal case rather than a corner:
    -- both copies of a shadowed header subtract to the same string. What an
    -- argument resolves to depends only on the argument, so ask once each.
    resolved <- resolveHeaderNames (contramap HeaderNameClang tracer) args root
                  . nubOrd $ map ByBracket (concatMap snd candidates)

    let reaches :: C.HashIncludeArg -> Maybe FileId
        reaches arg = Map.findWithDefault Nothing (ByBracket arg) resolved

    fmap Map.fromList . forM candidates $ \(file, candidateArgs) ->
      case List.find ((== Just file) . reaches) candidateArgs of
        Just arg -> return (file, ByBracket arg)
        Nothing  -> do
          -- Running out of candidates has two causes and only one is worth
          -- reporting. A candidate that reached some other file says the name
          -- exists and is taken, which is shadowing. A candidate that reached
          -- nothing says only that this header has no bracket name, which is
          -- ordinary for a library that includes its own internals by quote.
          let name  = ByQuote (quoteName root file)
              taken = listToMaybe [ (arg, by)
                                  | arg     <- candidateArgs
                                  , Just by <- [reaches arg]
                                  ]
          forM_ taken $ \(arg, by) ->
            traceWith tracer $ withCallStack $ HeaderShadowed arg by name
          return (file, name)
  where
    canonicalDir :: CIncludeDir -> IO FilePath
    canonicalDir = Dir.canonicalizePath . getCIncludeDir

-- | The names worth trying for one file, best first
--
-- The candidates are the proper suffixes of the file's own path. That is the
-- whole set: the search path we can see is only the part that arrived as an
-- argument, and @clang@ also searches directories built into it, which is
-- where the C standard library lives. Guessing from the path covers both.
--
-- The search path does not add to that set, it orders it. Subtracting a
-- containing directory strips a /prefix/, which leaves a /suffix/ we already
-- had, so those names move to the front rather than joining the list.
--
-- Front in search path order, because a name taken from directory @D@ can only
-- be lost later to a directory /before/ @D@ gaining the same relative path.
-- The earlier the directory, the fewer ways there are to lose it, and these
-- names are read back from binding specifications against a tree that has
-- moved on. The rest follow shortest first, which is all we can say about a
-- name we cannot place.
candidatesFor :: [FilePath] -> FileId -> [C.HashIncludeArg]
candidatesFor dirs file =
    [ C.HashIncludeArg (toIncludeSyntax rel)
    | rel <- nubOrd (fromSearchPath ++ suffixes)
    ]
  where
    segments :: [FilePath]
    segments = FilePath.splitDirectories file.path

    fromSearchPath :: [FilePath]
    fromSearchPath = [
        FilePath.joinPath rest
      | dir <- dirs
      , Just rest@(_:_) <- [List.stripPrefix (FilePath.splitDirectories dir) segments]
      ]

    -- Proper suffixes, shortest first: "types.h", "bits/types.h", and so on.
    --
    -- Proper matters. The improper one is the whole segment list, which joins
    -- back to the absolute path, and an absolute path always resolves. Keeping
    -- it would give every file a bracket name and 'headerNamesOf' would never
    -- reach its quote fallback, so a shadowed header would be named after this
    -- machine instead of after the project.
    suffixes :: [FilePath]
    suffixes = [
        FilePath.joinPath suffix
      | n <- [1 .. length segments - 1]
      , let suffix = drop (length segments - n) segments
      ]

-- | The file's path from the project root, as a quote include argument
--
-- Falls back to the absolute path when the file lies outside the root, which
-- still resolves and is still unique, at the cost of naming this machine.
quoteName :: ProjectRoot -> FileId -> C.HashIncludeArg
quoteName (ProjectRoot root) file =
    C.HashIncludeArg . toIncludeSyntax $
      FilePath.makeRelative root file.path

-- | @#include@ arguments are C syntax, so they separate with forward slashes
toIncludeSyntax :: FilePath -> FilePath
toIncludeSyntax = map (\c -> if FilePath.isPathSeparator c then '/' else c)

{-------------------------------------------------------------------------------
  Internal auxiliary: batched resolution
-------------------------------------------------------------------------------}

-- | Resolve header names back to the files they name
--
-- This is the round trip that specifies a t'HeaderName', so it is also how
-- 'headerNamesOf' decides whether to keep a candidate.
--
-- The two constructors are asked separately, since they claim different things
-- about where the including file sits. A bracket name has to resolve from
-- anywhere, so its probe goes in a directory we do not expect to exist:
-- @clang@ searches the including file's own directory as well as the search
-- path, and a probe sitting anywhere real would accept names that only work
-- from there. A quote name claims only to resolve from the project root, so
-- its probe sits exactly there.
--
-- One parse per group, with the line number tying each answer back to the name
-- that produced it. A name that does not resolve, or that reaches a file with
-- no real path, maps to 'Nothing'.
resolveHeaderNames ::
     Tracer ClangMsg
  -> ClangArgs
  -> ProjectRoot
  -> [HeaderName]
  -> IO (Map HeaderName (Maybe FileId))
resolveHeaderNames tracer args (ProjectRoot root) names =
    Map.union
      <$> resolveGroup nowhere    [ n | n@ByBracket{} <- names ]
      <*> resolveGroup projectDir [ n | n@ByQuote{}   <- names ]
  where
    -- Deliberately not a directory we expect to find on disk.
    nowhere, projectDir :: FilePath
    nowhere    = root FilePath.</> ".hs-bindgen-header-name-probe"
    projectDir = root

    -- One parse for the group, then read the answers off by line number.
    resolveGroup :: FilePath -> [HeaderName] -> IO (Map HeaderName (Maybe FileId))
    resolveGroup dir group
      | null group = return Map.empty
      | otherwise  = do
          byLine <- includedFilesByLine tracer (clangSetup dir group)
                      (rootHeaderPath dir)
          return $ Map.fromList [
              (name, Map.findWithDefault Nothing line byLine)
            | (line, name) <- zip [1 ..] group
            ]

    rootHeaderName :: FilePath -> FilePath
    rootHeaderName dir = dir FilePath.</> "hs-bindgen-header-name.h"

    rootHeaderPath :: FilePath -> SourcePath
    rootHeaderPath = SourcePath . Text.pack . rootHeaderName

    clangSetup :: FilePath -> [HeaderName] -> ClangSetup
    clangSetup dir group =
        defaultClangSetup args $
          ClangInputMemory (rootHeaderName dir)
            (unlines (map C.renderHeaderName group))

-- | Which file each @#include@ in the given file reached, by line number
--
-- Directives the parsed file pulls in are skipped, so the result describes only
-- the lines we wrote. A line whose directive resolved to nothing, or to a file
-- with no real path, is absent.
includedFilesByLine ::
     Tracer ClangMsg
  -> ClangSetup
  -> SourcePath  -- ^ the file whose directives to report on
  -> IO (Map Int (Maybe FileId))
includedFilesByLine tracer setup wanted =
    fmap (Map.fromList . fromMaybe []) . withClang' tracer setup $ \unit -> do
      rootCursor <- clang_getTranslationUnitCursor unit
      fmap Just . HighLevel.clang_visitChildren rootCursor $ simpleFold $ \curr -> do
        mKind <- fromSimpleEnum <$> clang_getCursorKind curr
        case mKind of
          Right CXCursor_InclusionDirective -> do
            sloc <- HighLevel.clang_getCursorLocation' curr
            if singleLocPath sloc /= wanted
              then foldContinue
              else do
                file <- fileIdOf =<< clang_getIncludedFile curr
                foldContinueWith (singleLocLine sloc, file)
          _otherwise -> foldContinue
