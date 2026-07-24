-- | Golden tests: macro fixtures run with the 'Macro.Empty' and 'Macro.Raw'
--   macro languages.
--
-- These reuse the existing macro fixtures, but run them with the 'Empty' and
-- 'Raw' macro languages. Output is stored alongside the @c-expr@ fixtures with
-- test variant suffixes.
--
-- Trace messages are tolerated wholesale ('tolerateAll'): the 'Empty' and 'Raw'
-- languages produces different traces than @c-expr@ (no macro typechecking or
-- reparsing, but the same non-macro traces such as select conflicts), so the
-- per-test @c-expr@ predicates do not carry over. The point of these tests is
-- to compare the /generated bindings/.
module Test.HsBindgen.Golden.Macros.Lang (testCases) where

import HsBindgen.Imports

import Test.Common.HsBindgen.Trace.Predicate
import Test.HsBindgen.Golden.Infra.TestCase
import Test.HsBindgen.Golden.Infra.TestCaseTree
import Test.HsBindgen.Golden.Macros qualified as Macros
import Test.HsBindgen.Golden.Macros.Redeclaration qualified as Redeclaration
import Test.HsBindgen.Golden.Macros.Reparse qualified as Reparse

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

testCases :: [TestCaseTree]
testCases =
    [ TestCaseSection "empty" $ TestCaseLeafs $ map emptyVariant macroTestCases
    , TestCaseSection "raw"   $ TestCaseLeafs $ map rawVariant   macroTestCases
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
