{-# OPTIONS_GHC -Wno-orphans #-}

module Test.HsBindgen.Prop.Selection (tests) where

import Data.String (IsString (fromString))
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), CoArbitrary (coarbitrary),
                              Fun, Function (function),
                              NonNegative (getNonNegative), Property,
                              coarbitraryShow, elements, functionMap, oneof,
                              pattern Fn, testProperty, (=/=), (===))

import Clang.HighLevel.Types
import Clang.Paths
import HsBindgen.C.Predicate
import HsBindgen.Language.C qualified as C
import HsBindgen.Frontend.Naming

tests :: TestTree
tests = testGroup "Test.HsBindgen.Prop.Selection" [
          testGroup "match" [
            testProperty "all"                    prop_selectAll
          , testProperty "none"                   prop_selectNone
          , testProperty "and"                    prop_selectIfBoth
          , testProperty "or"                     prop_selectIfEither
          , testProperty "negate"                 prop_selectNegate
          , testProperty "from-main-files"        prop_selectFromMainFiles
          , testProperty "by-file-name/all"       prop_selectByFileNameAll
          , testProperty "by-file-name/needle"    prop_selectByFileNameNeedle
          , testProperty "by-element-name/all"    prop_selectByElementNameAll
          , testProperty "by-element-name/needle" prop_selectByElementNameNeedle
          ]
        , testGroup "merge" [
            testProperty "select/none"     prop_mergeSelectNone
          , testProperty "select/add/all"  prop_mergeAddSelectAll
          , testProperty "select/add/none" prop_mergeAddSelectNone
          , testCase     "all/pos"         mergeAllPos
          , testCase     "all/neg"         mergeAllNeg
          , testCase     "skip/one"        mergeSkipOne
          , testCase     "skip/two"        mergeSkipTwo
          ]
        ]

{-------------------------------------------------------------------------------
  Selection properties
-------------------------------------------------------------------------------}

prop_selectAll :: SingleLoc -> QualPrelimDeclId -> Bool
prop_selectAll loc qid = match (const True) loc qid SelectAll

prop_selectNone :: SingleLoc -> QualPrelimDeclId -> Bool
prop_selectNone loc qid = not $ match (const True) loc qid SelectNone

prop_selectIfBoth
  :: Fun SingleLoc Bool -> SingleLoc -> QualPrelimDeclId
  -> Predicate -> Predicate -> Bool
prop_selectIfBoth (Fn isMainFile) loc qid p1 p2 =
  let p1Res = match isMainFile loc qid p1
      p2Res = match isMainFile loc qid p2
      p1AndP2Res = match isMainFile loc qid (SelectIfBoth p1 p2)
   in (p1Res && p2Res) == p1AndP2Res

prop_selectIfEither
  :: Fun SingleLoc Bool -> SingleLoc -> QualPrelimDeclId
  -> Predicate -> Predicate -> Bool
prop_selectIfEither (Fn isMainFile) loc qid p1 p2 =
  let p1Res = match isMainFile loc qid p1
      p2Res = match isMainFile loc qid p2
      p1AndP2Res = match isMainFile loc qid (SelectIfEither p1 p2)
   in (p1Res || p2Res) == p1AndP2Res

prop_selectNegate
  :: Fun SingleLoc Bool -> SingleLoc -> QualPrelimDeclId -> Predicate
  -> Property
prop_selectNegate (Fn isMainFile) loc qid predicate =
      match isMainFile loc qid predicate
  =/= match isMainFile loc qid (SelectNegate predicate)

prop_selectFromMainFiles
  :: Fun SingleLoc Bool -> SingleLoc -> QualPrelimDeclId -> Bool
prop_selectFromMainFiles (Fn isMainFile) loc qid =
  match isMainFile loc qid SelectFromMainFiles == isMainFile loc

prop_selectByFileNameAll
  :: Fun SingleLoc Bool -> SingleLoc -> QualPrelimDeclId -> Bool
prop_selectByFileNameAll (Fn isMainFile) loc qid =
  match isMainFile loc qid (SelectByFileName ".*")

prop_selectByFileNameNeedle
  :: Fun SingleLoc Bool -> SingleLoc -> QualPrelimDeclId -> Bool
prop_selectByFileNameNeedle (Fn isMainFile) loc qid =
  let (SourcePath sourcePath) = singleLocPath loc
      sourcePath' = sourcePath <> "NEEDLE" <> sourcePath
      loc' = loc { singleLocPath = SourcePath sourcePath'}
   in match isMainFile loc' qid (SelectByFileName "NEEDLE")

prop_selectByElementNameAll
  :: Fun SingleLoc Bool -> SingleLoc -> QualPrelimDeclId -> Bool
prop_selectByElementNameAll (Fn isMainFile) loc qid =
    maybeNot $ match isMainFile loc qid (SelectByElementName ".*")
  where
    maybeNot :: (Bool -> Bool)
    maybeNot = case qid of
      QualPrelimDeclIdNamed{} -> id
      _othewise               -> not

prop_selectByElementNameNeedle
  :: Fun SingleLoc Bool -> SingleLoc -> QualPrelimDeclId -> Bool
prop_selectByElementNameNeedle (Fn isMainFile) loc = \case
    QualPrelimDeclIdNamed name kind ->
      let name' = name <> "NEEDLE" <> name
          qid'  = QualPrelimDeclIdNamed name' kind
       in match isMainFile loc qid' (SelectByElementName "NEEDLE")
    _otherwise ->
      True -- skip

{-------------------------------------------------------------------------------
  Match tests and properties
-------------------------------------------------------------------------------}

prop_mergeSelectNone :: [Predicate] -> Property
prop_mergeSelectNone ps = mergePredicates ps [] === SelectNone

prop_mergeAddSelectAll :: [Predicate] -> [Predicate] -> Property
prop_mergeAddSelectAll ps qs =
  mergePredicates ps [SelectAll] === mergePredicates ps (SelectAll : qs)

prop_mergeAddSelectNone :: [Predicate] -> [Predicate] -> Property
prop_mergeAddSelectNone ps qs =
  mergePredicates ps qs === mergePredicates (SelectNone : ps) qs

mergeAllPos, mergeAllNeg :: Assertion
mergeAllPos = mergePredicates [] [SelectAll] @?= SelectAll
mergeAllNeg = mergePredicates [SelectNone] [SelectAll] @?= SelectAll

mergeSkipOne :: Assertion
mergeSkipOne = mergePredicates [SelectByElementName "a"] [SelectAll]
                @?= SelectNegate (SelectByElementName "a")

mergeSkipTwo :: Assertion
mergeSkipTwo = mergePredicates [pa, pb] [SelectAll]
                 @?= SelectIfBoth (SelectNegate pa) (SelectNegate pb)
  where pa = SelectByElementName "a"
        pb = SelectByElementName "b"

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

instance Arbitrary SourcePath where
  arbitrary = SourcePath . Text.pack <$> arbitrary

instance Function SourcePath where
  function = functionMap
               (\(SourcePath t) -> Text.unpack t)
               (SourcePath . Text.pack)

instance CoArbitrary SourcePath where
  coarbitrary = coarbitraryShow

instance Arbitrary SingleLoc where
  arbitrary = SingleLoc
    <$> arbitrary
    <*> (getNonNegative <$> arbitrary)
    <*> (getNonNegative <$> arbitrary)

instance Function SingleLoc

instance CoArbitrary SingleLoc

instance Arbitrary C.Name where
  arbitrary = C.Name . Text.pack <$> arbitrary

instance Arbitrary C.NameKind where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary QualPrelimDeclId where
  -- TODO: We currently never produce anonymous or builtin declarations.
  -- In this module we check that selection predicates behave as boolean
  -- functions; this is not true for builtins (which are /never/ selected).
  arbitrary = QualPrelimDeclIdNamed <$> arbitrary <*> arbitrary

instance Arbitrary Predicate where
  arbitrary = oneof [
                  pure SelectAll
                , SelectIfBoth <$> arbitrary <*> arbitrary
                , SelectNegate <$> arbitrary
                , pure SelectFromMainFiles
                , SelectByFileName <$> elements regexPatterns
                , SelectByElementName <$> elements regexPatterns
                ]

regexPatterns :: [Regex]
regexPatterns = map fromString
  [ -- Basic literals and character classes
    "abc"
  , "\\d+"
  , "\\w*"
  , "\\s?"
  , "."
  , "[aeiou]"
  , "[^0-9]"
  , "[a-z0-9]+"

    -- Anchors and word boundaries
  , "^start"
  , "end$"
  , "\\bword\\b"

    -- Quantifiers
  , "colou?r"
  , "a{3}"
  , "b{1,3}"

    -- Alternation and grouping
  , "cat|dog"
  , "gr(a|e)y"
  , "(abc)+"

    -- Escaping and specific common patterns
  , "foo\\.bar"
  , "https?://"
  , "[A-Za-z]+"
  , "\\d{4}-\\d{2}-\\d{2}"

    -- Edge cases
  , ""
  , "^$"
  ]
