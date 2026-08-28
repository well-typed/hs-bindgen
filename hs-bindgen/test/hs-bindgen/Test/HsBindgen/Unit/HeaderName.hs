module Test.HsBindgen.Unit.HeaderName (tests) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import System.Directory qualified as Dir
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import Clang.Args
import Clang.Paths

import HsBindgen.Config.ClangArgs
import HsBindgen.Frontend.Analysis.HeaderName (ProjectRoot (..))
import HsBindgen.Frontend.Analysis.HeaderName qualified as HeaderName
import HsBindgen.Imports
import HsBindgen.IR.C (FileId, HeaderName (..))
import HsBindgen.IR.C qualified as C
import HsBindgen.Util.Tracer

import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  List of tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests getTestResources = testGroup "Test.HsBindgen.Unit.HeaderName" [
      testCase "unambiguous header keeps a bracket name" $
        testUnambiguous getTestResources
    , testCase "shadowed header falls back to a quote name" $
        testShadowing getTestResources
    , testCase "three roots stay distinguishable" $
        testThreeRoots getTestResources
    , testCase "candidates are ordered by search path position" $
        testCandidateOrder getTestResources
    , testCase "file outside every search directory" $
        testOutside getTestResources
    , testCase "every name resolves back to the file it names" $
        testRoundTrip getTestResources
    , testCase "a shadowed header cannot be keyed as the shadowing one" $
        testShadowingSpecKeys getTestResources
    , testCase "a persisted name reads back as itself" testKeyRoundTrip
    ]

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | A header no other search directory shadows is named by bracket
testUnambiguous :: IO TestResources -> Assertion
testUnambiguous getTestResources = do
    env <- mkEnv getTestResources ["vendor", "include"]
    onlyHere <- fileIn env ("include" </> "widget" </> "only_here.h")
    names <- nameAll env [onlyHere]
    assertEqual "only_here.h"
      (Just (ByBracket (C.HashIncludeArg "widget/only_here.h")))
      (Map.lookup onlyHere names)

-- | Two search directories holding the same relative path
--
-- The one that comes first wins the bracket name. The other cannot have it,
-- since including that name finds its rival, so it is named from the project
-- root instead. The two names differ, which is what stops a binding spec
-- written for one from being applied to the other.
testShadowing :: IO TestResources -> Assertion
testShadowing getTestResources = do
    env <- mkEnv getTestResources ["vendor", "include"]
    shadowing <- fileIn env ("vendor"  </> "widget" </> "core.h")
    shadowed  <- fileIn env ("include" </> "widget" </> "core.h")
    names <- nameAll env [shadowing, shadowed]

    assertEqual "shadowing copy takes the bracket name"
      (Just (ByBracket (C.HashIncludeArg "widget/core.h")))
      (Map.lookup shadowing names)

    case Map.lookup shadowed names of
      Just (ByQuote arg) ->
        assertEqual "shadowed copy is named from the project root"
          (relativeToRoot env shadowed) arg
      other ->
        assertFailure $ "expected a quote name, got " ++ show other

    assertBool "the two names differ" $
      Map.lookup shadowing names /= Map.lookup shadowed names

-- | The same relative path in three search directories
--
-- A name relative to the search directory would call all three @core.h@ and
-- collide. Naming from the project root keeps them apart.
testThreeRoots :: IO TestResources -> Assertion
testThreeRoots getTestResources = do
    env <- mkEnv getTestResources ["vendor", "include", "extra"]
    files <- mapM (fileIn env)
      [ "vendor"  </> "widget" </> "core.h"
      , "include" </> "widget" </> "core.h"
      , "extra"   </> "widget" </> "core.h"
      ]
    names <- nameAll env files
    let assigned = mapMaybe (`Map.lookup` names) files
    assertEqual "every file was named" 3 (length assigned)
    assertEqual "all three names are distinct" 3
      (Set.size (Set.fromList assigned))

-- | Nested search directories, run in both orders
--
-- Both @widget\/core.h@ and @core.h@ name the same file here, so the choice is
-- a preference rather than a correctness question. We take the candidate from
-- the earliest search directory, because that is the one a file added later
-- has the fewest ways to capture.
testCandidateOrder :: IO TestResources -> Assertion
testCandidateOrder getTestResources = do
    broadFirst <- mkEnv getTestResources ["include", "include/widget"]
    file <- fileIn broadFirst ("include" </> "widget" </> "core.h")
    namesBroad <- nameAll broadFirst [file]
    assertEqual "broad directory first"
      (Just (ByBracket (C.HashIncludeArg "widget/core.h")))
      (Map.lookup file namesBroad)

    narrowFirst <- mkEnv getTestResources ["include/widget", "include"]
    namesNarrow <- nameAll narrowFirst [file]
    assertEqual "narrow directory first"
      (Just (ByBracket (C.HashIncludeArg "core.h")))
      (Map.lookup file namesNarrow)

-- | A file no search directory covers still gets a name
testOutside :: IO TestResources -> Assertion
testOutside getTestResources = do
    env <- mkEnv getTestResources ["vendor", "include"]
    lone <- fileIn env ("outside" </> "lone.h")
    names <- nameAll env [lone]
    case Map.lookup lone names of
      Just (ByQuote arg) ->
        assertEqual "named from the project root"
          (relativeToRoot env lone) arg
      other ->
        assertFailure $ "expected a quote name, got " ++ show other

-- | The specification: including a name finds the file it names
--
-- Covers both constructors, since the fixture has a shadowed header and a
-- header outside the search path as well as ordinary ones.
testRoundTrip :: IO TestResources -> Assertion
testRoundTrip getTestResources = do
    env <- mkEnv getTestResources ["vendor", "include", "extra"]
    files <- mapM (fileIn env)
      [ "vendor"  </> "widget" </> "core.h"
      , "include" </> "widget" </> "core.h"
      , "extra"   </> "widget" </> "core.h"
      , "include" </> "widget" </> "only_here.h"
      , "outside" </> "lone.h"
      ]
    names <- nameAll env files
    assertEqual "every file was named" (length files) (Map.size names)

    resolved <- HeaderName.resolveHeaderNames
      nullTracer env.clangArgs env.projectRoot (Map.elems names)

    forM_ (Map.toList names) $ \(file, name) ->
      assertEqual ("resolving " ++ show name)
        (Just (Just file)) (Map.lookup name resolved)

-- | The names are what a binding specification is keyed on, so they must differ
--
-- This is the case where getting a name wrong is not cosmetic. Both copies
-- declare @header_name_core@ with different field types, so keying the shadowed
-- copy under the shadowing copy's name would bind one struct with the other's
-- layout. Checked in both flag orders, since which copy is shadowed is
-- precisely what the order decides.
testShadowingSpecKeys :: IO TestResources -> Assertion
testShadowingSpecKeys getTestResources =
    forM_ [("vendor", "include"), ("include", "vendor")] $ \(earlier, later) -> do
      let order = [earlier, later]
      env     <- mkEnv getTestResources order
      first'  <- fileIn env (earlier </> "widget" </> "core.h")
      second' <- fileIn env (later   </> "widget" </> "core.h")
      names   <- nameAll env [first', second']

      assertEqual (show order ++ ": the earlier directory takes the bracket name")
        (Just (ByBracket (C.HashIncludeArg "widget/core.h")))
        (Map.lookup first' names)

      assertBool (show order ++ ": the two keys differ") $
        Map.lookup first' names /= Map.lookup second' names

      -- Not merely different: the shadowed one must not be a bracket name at
      -- all, or it would claim to be reachable as something it is not.
      case Map.lookup second' names of
        Just ByQuote{} -> return ()
        other          -> assertFailure $
          show order ++ ": expected a quote name for the shadowed copy, got "
            ++ show other

-- | What a binding specification stores reads back as what it stored
--
-- A bracket name is written bare so that specifications predating quote names
-- keep their meaning, which only works while a bare name cannot be mistaken
-- for a delimited one. Both delimiters are legal in a filename, so the awkward
-- names are the point of this test rather than a curiosity.
testKeyRoundTrip :: Assertion
testKeyRoundTrip =
    forM_ names $ \name ->
      assertEqual (show name) name $
        C.parseHeaderName (C.headerNameKey name).path
  where
    names :: [HeaderName]
    names = concat [
        [ctor (C.HashIncludeArg arg) | ctor <- [ByBracket, ByQuote]]
      | arg <- [
            "core.h"
          , "widget/core.h"
          , "../core.h"
            -- Names that already look like a directive.
          , "<core.h>"
          , "\"core.h\""
            -- One delimiter, unbalanced.
          , "<core.h"
          , "core.h>"
          , "\"core.h"
            -- Degenerate.
          , "<"
          , "\""
          , ""
          ]
      ]

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- | Everything a naming run needs, for one choice of search path
data Env = Env {
      clangArgs   :: ClangArgs
    , projectRoot :: ProjectRoot
    , incDirs     :: [CIncludeDir]
    , fixtureRoot :: FilePath
    }

-- | Build an t'Env' whose search path is the given fixture subdirectories
--
-- The project root is the fixture directory rather than the working directory,
-- so the quote names in these tests do not depend on where the suite is run
-- from.
mkEnv :: IO TestResources -> [FilePath] -> IO Env
mkEnv getTestResources dirs = do
    testResources <- getTestResources
    let fixtureRoot =
          testResources.packageRoot </> "test-artefacts" </> "headers"
            </> "header-name"
        config = getTestClangArgsConfig c99
          (map (("test-artefacts" </> "headers" </> "header-name") </>) dirs)
          testResources
    root <- Dir.canonicalizePath fixtureRoot
    return Env{
        clangArgs   = clangArgsConfigToClangArgs config
      , projectRoot = ProjectRoot root
      , incDirs     = map CIncludeDir config.extraIncludeDirs
      , fixtureRoot = root
      }

-- | The t'FileId' of a fixture file, given its path below the fixture root
fileIn :: Env -> FilePath -> IO FileId
fileIn env rel =
    C.FileId <$> Dir.canonicalizePath (env.fixtureRoot </> rel)

nameAll :: Env -> [FileId] -> IO (Map FileId HeaderName)
nameAll env =
    HeaderName.headerNamesOf nullTracer env.clangArgs env.projectRoot env.incDirs

-- | What a quote name for this file should say
relativeToRoot :: Env -> FileId -> C.HashIncludeArg
relativeToRoot env file =
    C.HashIncludeArg $ makeRelative' env.fixtureRoot file.path
  where
    makeRelative' root path =
      let n = length root
      in  if take n path == root then drop (n + 1) path else path
