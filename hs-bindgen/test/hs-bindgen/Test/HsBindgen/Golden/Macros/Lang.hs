-- | Golden tests: macro fixtures run with the 'Macro.Empty' and 'Macro.Raw'
--   macro languages.
--
-- These reuse the existing macro fixtures, but run them with the 'Empty' and
-- 'Raw' macro languages. Output is stored alongside the @c-expr@ fixtures with
-- test variant suffixes.
--
-- Trace messages are tolerated wholesale ('tolerateAll'): the 'Empty' and 'Raw'
-- languages produce different traces than @c-expr@ (no macro typechecking or
-- reparsing, but the same non-macro traces such as select conflicts), so the
-- per-test @c-expr@ predicates do not carry over. The point of these tests is
-- to compare the /generated bindings/.
module Test.HsBindgen.Golden.Macros.Lang (testCases) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)

import HsBindgen (getBindingsMultiple)
import HsBindgen.Backend.Category
import HsBindgen.Config (ModuleRenderConfig (..))
import HsBindgen.Config.Internal (FrontendConfig (..))
import HsBindgen.Frontend.Pass.ApplyMacroFallback.IsPass (MacroFallback (..))
import HsBindgen.Frontend.Predicate (Boolean (..))
import HsBindgen.Imports

import Test.Common.HsBindgen.Trace.Predicate
import Test.HsBindgen.Golden.Infra.TestCase
import Test.HsBindgen.Golden.Infra.TestCaseTree
import Test.HsBindgen.Golden.Macros qualified as Macros
import Test.HsBindgen.Golden.Macros.Redeclaration qualified as Redeclaration
import Test.HsBindgen.Golden.Macros.Reparse qualified as Reparse
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

testCases :: [TestCaseTree]
testCases =
    [ TestCaseSection "empty" $ TestCaseLeafs $ map emptyVariant macroTestCases
    , TestCaseSection "raw"   $ TestCaseLeafs $ map rawVariant   macroTestCases
    , TestCaseSection "empty-fallback-is-raw" [
          TestCaseCustom test checkEmptyFallbackIsRaw
        | test <- macroTestCases
        ]
    ]
  where
    emptyVariant = withMacroLangVariant 1 "empty" macroLangEmpty
    rawVariant   = withMacroLangVariant 2 "raw"   macroLangRaw

-- | The macro fixtures we reuse, in their @c-expr@ form.
macroTestCases :: [TestCase]
macroTestCases = concat [
      Macros.testCases
    , concatMap flatten Reparse.testCases
    , concatMap flatten Redeclaration.testCases
    ]

-- | Turn a @c-expr@ macro test into another language variant.
--
-- Parsing configuration (C standard, @libclang@ version constraints, frontend
-- tweaks, binding specs) is preserved, since it is independent of the macro
-- language; only the macro language, output location and trace predicate
-- change.
withMacroLangVariant :: Natural -> String -> SomeMacroLang -> TestCase -> TestCase
withMacroLangVariant index suffix macroLang test =
    mkTestVariant (Just index) suffix test
      & #macroLang      .~ macroLang
      & #tracePredicate .~ tolerateAll

{-------------------------------------------------------------------------------
  Empty with fallback is Raw
-------------------------------------------------------------------------------}

-- | 'Empty' gives up on every macro, so with the fallback for all macros it
-- generates the bindings of 'Raw'
checkEmptyFallbackIsRaw :: IO TestResources -> TestCase -> TestTree
checkEmptyFallbackIsRaw getTestResources test = testCase test.name $ do
    raw      <- generate $ test & #macroLang .~ macroLangRaw
    fallback <- generate $ test
      & #macroLang  .~ macroLangEmpty
      & #onFrontend %~ (( #macroFallback .~ MacroFallback BTrue ) .)
    forM_ allCategories $ \bc ->
      case ( lines <$> view (lensForCategory bc) raw
           , lines <$> view (lensForCategory bc) fallback
           ) of
        (Nothing, Nothing) -> pure ()
        (Just r, Just f)
          | Just diff <- firstDifference r f ->
              assertFailure $ show bc ++ ": " ++ diff
          | otherwise -> pure ()
        (r, _) -> assertFailure $ concat [
            show bc ++ ": module generated with "
          , if isJust r then "Raw" else "Empty with fallback"
          , " only"
          ]
  where
    generate :: TestCase -> IO (ByCategory_ (Maybe String))
    generate test' =
        runTestHsBindgenSuccess (\_ -> pure ()) getTestResources
          (test' & #tracePredicate .~ tolerateAll)
          (getBindingsMultiple ModuleRenderConfig{
               qualifiedStyle = test.qualifiedStyle
             })

-- | The first line that differs, if any
firstDifference :: [String] -> [String] -> Maybe String
firstDifference = go (1 :: Int)
  where
    go _ []     []     = Nothing
    go n (r:rs) (f:fs)
      | r == f         = go (n + 1) rs fs
    go n rs     fs     = Just $ unlines [
        "first difference at line " ++ show n
      , "  Raw:                 " ++ showLine rs
      , "  Empty with fallback: " ++ showLine fs
      ]

    showLine :: [String] -> String
    showLine []    = "<end of module>"
    showLine (l:_) = l
