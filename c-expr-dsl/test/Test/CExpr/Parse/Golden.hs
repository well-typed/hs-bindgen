-- | Golden integration tests for 'C.Expr.Parse.Expr.parseMacro'
--
-- These tests use @libclang@ to tokenise the macros defined in
-- @test/fixtures/macros.h@, feed the token streams to 'parseMacro', and
-- compare the results against the golden file @test/fixtures/macros.golden@.
--
-- Golden file can be regenerated using the @--accept@ CLI option.
module Test.CExpr.Parse.Golden (tests) where

import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import C.Expr.Parse
import C.Expr.Syntax

import Clang.Args
import Clang.CStandard
import Clang.Enum.Bitfield
import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import Clang.Version

import Paths_c_expr_dsl (getDataDir)

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
      case runtimeClangVersion of
        ClangVersion x | x >= (15,0,0) -> [goldenWith CExprC23]
        _                              -> []

goldenWith :: TestCStandard -> TestTree
goldenWith testCStd =
    goldenDynamic ("macros-" <> show cStd)
      (datadir </> "macros." <> show cStd <> ".golden")
      (parseMacrosFixture testCStd (datadir </> "macros.h"))
  where
    cStd = testCStandardToCStandard testCStd

{-# NOINLINE datadir #-}
datadir :: FilePath
datadir = unsafePerformIO getDataDir

-- | Minimal golden test using an 'IO' action to resolve the golden file path.
--
-- If the golden file does not yet exist it is created and the test fails so
-- that the developer can inspect and commit it. If it does exist the actual
-- output is compared byte-for-byte; on a mismatch the test fails with a hint
-- about how to regenerate the file.
goldenDynamic ::
     TestName
  -> FilePath           -- ^ path to the golden file
  -> IO LBS.ByteString  -- ^ action producing the actual output
  -> TestTree
goldenDynamic name goldenPath getActual = goldenVsString name goldenPath getActual

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
    formatResult (Right (Macro{macroExpr})) = "Right " ++ show macroExpr
    formatResult (Left _)                   = "Left <parse error>"

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
