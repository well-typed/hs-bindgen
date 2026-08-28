module Test.HsBindgen.Unit.IncludeGraph (tests) where

import Control.Exception (IOException, try)
import Data.List (isInfixOf, isSuffixOf)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import System.Directory (createDirectoryIfMissing, createFileLink)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Clang.Paths (getSourcePath)

import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph, SourceFile (..))
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.ProcessIncludes qualified as ProcessIncludes
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C

import Test.HsBindgen.Resources
import Test.HsBindgen.Unit.Frontend (execFrontendIncludeGraph)

{-------------------------------------------------------------------------------
  List of tests
-------------------------------------------------------------------------------}

-- | The fixture pairs two headers that include the same two files in opposite
-- orders. @clang@ keeps the name of a file's /first/ lookup, so the two orders
-- disagree about which spelling it reports, and everything below is about that
-- disagreement no longer being visible.
tests :: IO TestResources -> TestTree
tests getTestResources = testGroup "Test.HsBindgen.Unit.IncludeGraph" [
      testCase "one file is one vertex, whichever order" $
        testOneVertexPerFile getTestResources
    , testCase "header names do not depend on include order" $
        testNamesAgree getTestResources
    , testCase "a name reached through .. is the name that resolves" $
        testDotDotNormalised getTestResources
    , testCase "a name carries no directory the file is not in" $
        testNoPhantomDirectory getTestResources
    , testCase "only a symlink counts as an alias" testNamesFile
    , testCase "symlinks are reported only when they rename" testSymlinkShapes
    , testCase "a symlinked header is one file under both names" $
        testSymlinkedHeader getTestResources
    ]

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | @widget\/core.h@ is reached twice, by two spellings, and is one vertex
testOneVertexPerFile :: IO TestResources -> Assertion
testOneVertexPerFile getTestResources =
    forM_ ["other.h", "other_reversed.h"] $ \header -> do
      graph <- includeGraphOf getTestResources header
      let cores = filter (isCore graph) (IncludeGraph.toSortedList graph)
      assertEqual (header ++ ": vertices naming widget/core.h")
        1 (length cores)

-- | The two orders produce the same names
--
-- This is the property the whole design exists for: reordering two @#include@s
-- must not change what anything is called.
--
-- The two runs start from different root headers, so those two names differ by
-- construction and are not part of the claim. Everything they share is.
testNamesAgree :: IO TestResources -> Assertion
testNamesAgree getTestResources = do
    forward  <- namesOf getTestResources "other.h"
    reversed <- namesOf getTestResources "other_reversed.h"
    let shared = Set.delete "other.h" forward
    assertEqual "header names reached either way"
      shared (Set.delete "other_reversed.h" reversed)
    assertBool "the shared headers are actually there" $
      Set.fromList ["widget/core.h", "widget/gadget.h"] `Set.isSubsetOf` shared

-- | A file reached through a @..@ segment is named by a name that resolves
--
-- @widget\/gadget.h@ includes @\"..\/widget\/core.h\"@, so @clang@ may report
-- @core.h@ under a path with a @..@ in it. The name has to be the one an
-- @#include@ would actually find.
testDotDotNormalised :: IO TestResources -> Assertion
testDotDotNormalised getTestResources =
    forM_ ["other.h", "other_reversed.h"] $ \header -> do
      graph <- includeGraphOf getTestResources header
      let cores = [ name
                  | file      <- IncludeGraph.toSortedList graph
                  , isCore graph file
                  , Just name <- [IncludeGraph.headerNameOf graph file]
                  ]
      assertEqual (header ++ ": name of widget/core.h")
        [C.ByBracket (C.HashIncludeArg "widget/core.h")] cores

-- | A header path predicate must not see a directory the file is not in
--
-- @widget\/compat\/legacy.h@ reaches @core.h@ as @\"..\/core.h\"@, so the path
-- @clang@ reports for @core.h@ runs through @compat@. Matching a regex against
-- that path excludes @core.h@ from a @compat@ filter, which is wrong: the file
-- is in @widget@. The name has no such segment.
testNoPhantomDirectory :: IO TestResources -> Assertion
testNoPhantomDirectory getTestResources = do
    graph <- includeGraphOf getTestResources ("widget" </> "compat" </> "legacy.h")
    let cores = [ (C.headerNameArg name).path
                | file      <- IncludeGraph.toSortedList graph
                , isCore graph file
                , Just name <- [IncludeGraph.headerNameOf graph file]
                ]
    assertEqual "name of core.h" ["widget/core.h"] cores

    -- The reported path is what the old behaviour matched on, and it does
    -- mention compat. Without this the test above could pass for the wrong
    -- reason, on a fixture that never reproduced the problem.
    assertBool "clang did report core.h under a path running through compat" $
      any (\p -> "compat" `isInfixOf` getSourcePath p)
          [ p
          | (p, file) <- Map.toList graph.paths
          , isCore graph file
          ]

-- | Which @#include@ arguments count as naming the file they reached
--
-- Only an argument that does not name the file is an alias, and the check has
-- to be lexical on the argument. Comparing against the path @clang@ reported
-- would need it made absolute first, and the golden tests cannot catch that
-- mistake because they always pass @-I@ an absolute directory: with a relative
-- one, every header became an alias of itself.
testNamesFile :: Assertion
testNamesFile = do
    let real = C.FileId "/abs/include/widget/core.h"
    forM_ expected $ \(arg, names) ->
      assertEqual ("names " ++ arg)
        names (ProcessIncludes.namesFile real (C.HashIncludeArg arg))
  where
    expected :: [(FilePath, Bool)]
    expected = [
        -- The file's own name, however the directive spelled the route.
        ("widget/core.h",              True)
      , ("core.h",                     True)
      , ("../widget/core.h",           True)
      , ("./widget/core.h",            True)
      , ("include/widget/core.h",      True)
      , ("/abs/include/widget/core.h", True)
        -- A '..' cancels what precedes it. Merely dropping both parts would
        -- leave widget/widget/core.h here and call the file an alias.
      , ("widget/../widget/core.h",    True)
      , ("widget/compat/../core.h",    True)
      , ("../../include/widget/core.h", True)
        -- Some other name, which is what a symlink gives.
      , ("alias.h",                    False)
      , ("widget/alias.h",             False)
      , ("other/core.h",               False)
        -- Cancelling can land on another name too.
      , ("widget/../alias.h",          False)
        -- A suffix of a segment is not a segment.
      , ("re.h",                       False)
        -- A dot is only special as a whole segment. Hidden directories and
        -- names made of dots are ordinary.
      , ("widget/./core.h",            True)
      , (".hidden/core.h",             False)
      , ("widget/core.h/",             True)
      ]

-- | A symlink is reported only when it renames the file
--
-- The check is lexical, so it asks what the argument calls the file rather
-- than how the filesystem reached it. A link whose name matches its target's
-- is not an alias worth reporting: nothing was renamed.
testSymlinkShapes :: Assertion
testSymlinkShapes = do
    let real = C.FileId "/abs/vendor/same.h"
    assertBool "a link with the target's own name is not reported" $
      ProcessIncludes.namesFile real (C.HashIncludeArg "same.h")

    let nested = C.FileId "/abs/include/widget/core.h"
    assertBool "a link under another name is reported" $
      not (ProcessIncludes.namesFile nested (C.HashIncludeArg "widget/alias.h"))

    -- A symlinked search directory changes the real path's leading segments
    -- only, which the suffix check does not look at.
    assertBool "a symlinked search directory is not reported" $
      ProcessIncludes.namesFile
        (C.FileId "/abs/real_inc/widget/core.h")
        (C.HashIncludeArg "widget/core.h")

-- | A symlink gives one file, named as it resolves, with the written name kept
--
-- The tree is built here rather than checked in because CI rejects symbolic
-- links in the repository, to keep Windows checkouts simple (see
-- @scripts\/ci\/check-symlinks.sh@). Where a link cannot be created at all the
-- test says so and stops, since there is nothing left to observe.
testSymlinkedHeader :: IO TestResources -> Assertion
testSymlinkedHeader getTestResources =
    withSystemTempDirectory "hs-bindgen-symlink" $ \dir -> do
      let incDir = dir </> "include"
      createDirectoryIfMissing True (incDir </> "widget")
      writeFile (incDir </> "widget" </> "core.h") $ unlines [
          "#ifndef SYMLINK_CORE_H"
        , "#define SYMLINK_CORE_H"
        , "struct symlink_core { int x; };"
        , "#endif"
        ]
      writeFile (incDir </> "root.h") $ unlines [
          "#ifndef SYMLINK_ROOT_H"
        , "#define SYMLINK_ROOT_H"
        , "#include <alias.h>"
        , "#endif"
        ]
      linked <- try @IOException $
        createFileLink ("widget" </> "core.h") (incDir </> "alias.h")
      case linked of
        Left err -> putStrLn $
          "  (no symbolic links on this system, skipping: " ++ show err ++ ")"
        Right () -> do
          graph <- execFrontendIncludeGraph getTestResources c99 [incDir] "root.h"
          let cores = [ header
                      | header <- Map.elems graph.headers
                      , "widget/core.h" `isSuffixOf` (C.headerNameArg header.name).path
                      ]
          case cores of
            [core] -> do
              assertEqual "the name that resolves"
                (C.ByBracket (C.HashIncludeArg "widget/core.h")) core.name
              assertEqual "the name that was written"
                (Set.singleton (C.HashIncludeArg "alias.h")) core.aliases
            _otherwise ->
              assertFailure $
                "expected one header named widget/core.h, got " ++ show (length cores)

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

includeGraphOf :: IO TestResources -> FilePath -> IO IncludeGraph
includeGraphOf getTestResources header =
    execFrontendIncludeGraph getTestResources c99
      ["test-artefacts" </> "headers" </> "include-graph"]
      header

-- | Every header name in the graph, which is what must not move
namesOf :: IO TestResources -> FilePath -> IO (Set String)
namesOf getTestResources header = do
    graph <- includeGraphOf getTestResources header
    return $ Set.fromList
      [ (C.headerNameArg header').path
      | header' <- map (.name) (Map.elems graph.headers)
      ]

-- | Is this vertex the fixture's @widget\/core.h@?
--
-- Asked of the name rather than the real path, since the real path is this
-- machine's business.
isCore :: IncludeGraph -> SourceFile -> Bool
isCore graph file = case IncludeGraph.headerNameOf graph file of
    Just name  -> "widget/core.h" `isSuffixOf` (C.headerNameArg name).path
    Nothing    -> False
