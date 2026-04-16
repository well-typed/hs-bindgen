{-# LANGUAGE OverloadedStrings #-}

-- | Golden integration tests for 'C.Expr.Parse.Expr.parseMacro'
--
-- These tests use @libclang@ to tokenise the macros defined in
-- @test/fixtures/macros.h@, feed the token streams to 'parseMacro', and
-- compare the results against the golden file @test/fixtures/macros.golden@.
--
-- To regenerate the golden file, delete it and re-run the test suite.
module Test.CExpr.Parse.Golden (tests) where

import Control.Monad (unless, filterM)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure)

import Clang.Args
import Clang.CStandard
import Clang.Enum.Bitfield
import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import Clang.Version

import C.Expr.Parse
import C.Expr.Syntax
import Data.Bifunctor (Bifunctor(..))

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

data TestCStandard = CExprC17 | CExprC23

testCStandardToCStandard :: TestCStandard -> CStandard
testCStandardToCStandard = \case
    CExprC17 -> C17
    CExprC23 -> C23

testCStandardToClangArg :: TestCStandard -> String
testCStandardToClangArg = \case
    CExprC17 -> "-std=c17"
    CExprC23 -> "-std=c2x"

tests :: TestTree
tests = testGroup "Parse.Golden" $ [goldenWith CExprC17] ++ mbC23
  where
    mbC23 =
      case clangVersion of
        ClangVersion x | x >= (15,0,0) -> [goldenWith CExprC23]
        _                              -> []

goldenWith :: TestCStandard -> TestTree
goldenWith testCStd =
    goldenDynamic ("macros-" <> show cStd)
        (findFixturesDir >>= \dir ->
          pure $ dir </> "macros." <> show cStd <> ".golden")
        (findFixturesDir >>= \dir ->
          parseMacrosFixture testCStd (dir </> "macros.h"))
  where
    cStd = testCStandardToCStandard testCStd

-- | Find the fixtures directory, trying locations relative to the repo root
-- (when the test is run by @cabal test@ from the project root) and relative to
-- the package directory (when the test binary is run directly).
findFixturesDir :: IO FilePath
findFixturesDir = do
    let candidates = [
            "c-expr-dsl" </> "test" </> "fixtures"  -- from repo root
          , "test" </> "fixtures"                    -- from package root
          ]
    found <- filterM doesDirectoryExist candidates
    case found of
      (d : _) -> return d
      []      -> ioError $ userError $ unlines $
                   "Cannot find fixtures directory; tried:" :
                   map ("  " ++) candidates

-- | Minimal golden test using an 'IO' action to resolve the golden file path.
--
-- If the golden file does not yet exist it is created and the test fails so
-- that the developer can inspect and commit it. If it does exist the actual
-- output is compared byte-for-byte; on a mismatch the test fails with a hint
-- about how to regenerate the file.
goldenDynamic ::
     TestName
  -> IO FilePath        -- ^ path to the golden file (resolved at runtime)
  -> IO LBS.ByteString  -- ^ action producing the actual output
  -> TestTree
goldenDynamic name getGoldenPath getActual =
    testCase name $ do
        goldenPath <- getGoldenPath
        actual     <- getActual
        exists     <- doesFileExist goldenPath
        if exists
          then do
            expected <- LBS.readFile goldenPath
            unless (actual == expected) $
              assertFailure $ unlines [
                  "Output differs from golden file " ++ goldenPath ++ "."
                , "Delete the golden file and re-run to regenerate it."
                ]
          else do
            LBS.writeFile goldenPath actual
            assertFailure $
              "Golden file did not exist; created " ++ goldenPath ++
              ".\nRe-run the test suite to verify."

{-------------------------------------------------------------------------------
  Run the parser on all macros in the fixture file
-------------------------------------------------------------------------------}

parseMacrosFixture :: TestCStandard -> FilePath -> IO LBS.ByteString
parseMacrosFixture testCStd fixturePath = do
    macroTokens <- collectMacroTokens testCStd fixturePath
    return $ LBS.pack $ unlines $
      map (formatEntry . second (runParser $ parseMacro cStd)) macroTokens
  where
    cStd :: ClangCStandard
    cStd = ClangCStandard (testCStandardToCStandard testCStd) DisableGnu

    formatEntry :: (Text, Either MacroParseError Macro) -> String
    formatEntry (name, result) =
        Text.unpack name ++ ": " ++ formatResult result

    formatResult :: Either MacroParseError Macro -> String
    formatResult (Right mac) = "Right " ++ show (macroExpr mac)
    formatResult (Left _)    = "Left <parse error>"

{-------------------------------------------------------------------------------
  Collect macro definitions from a C header file via libclang
-------------------------------------------------------------------------------}

collectMacroTokens ::
     TestCStandard
  -> FilePath
  -> IO [(Text, [Token TokenSpelling])]
collectMacroTokens testCStd path =
    HighLevel.withIndex DontDisplayDiagnostics $ \index ->
      HighLevel.withTranslationUnit index src noArgs [] flags $ \unit -> do
        root    <- clang_getTranslationUnitCursor unit
        HighLevel.clang_visitChildren root (macroFold unit)
  where
    src :: Maybe SourcePath
    src = Just $ SourcePath $ Text.pack path

    noArgs :: ClangArgs
    noArgs = ClangArgs [testCStandardToClangArg testCStd]

    flags :: BitfieldEnum CXTranslationUnit_Flags
    flags = bitfieldEnum [CXTranslationUnit_DetailedPreprocessingRecord]

macroFold ::
     CXTranslationUnit
  -> Fold IO (Text, [Token TokenSpelling])
macroFold unit = simpleFold $ \cursor -> do
    loc    <- clang_getCursorLocation cursor
    inMain <- clang_Location_isFromMainFile loc
    if not inMain
      then foldContinue
      else do
        kind <- fromSimpleEnum <$> clang_getCursorKind cursor
        case kind of
          Right CXCursor_MacroDefinition -> do
              name   <- clang_getCursorSpelling cursor
              range  <- HighLevel.clang_getCursorExtent cursor
              tokens <- HighLevel.clang_tokenize unit (multiLocExpansion <$> range)
              foldContinueWith (name, tokens)
          _ ->
              foldContinue
