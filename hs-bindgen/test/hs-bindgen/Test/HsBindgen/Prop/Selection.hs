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
import HsBindgen.Frontend.Naming qualified as C

tests :: TestTree
tests = testGroup "Test.HsBindgen.Prop.Selection" [
      testGroup "matchParse" [
          testProperty "true"                  prop_parseTrue
        , testProperty "false"                 prop_parseFalse
        , testProperty "and"                   prop_parseAnd
        , testProperty "or"                    prop_parseOr
        , testProperty "not"                   prop_parseNot
        , testProperty "from-main-headers"     prop_parseSelectFromMainHeaders
        , testProperty "by-header-path/all"    prop_parseSelectByHeaderPathAll
        , testProperty "by-header-path/needle" prop_parseSelectByHeaderPathNeedle
        ]
    , testGroup "matchSelect" [
          testProperty "true"                  prop_selectTrue
        , testProperty "false"                 prop_selectFalse
        , testProperty "and"                   prop_selectAnd
        , testProperty "or"                    prop_selectOr
        , testProperty "not"                   prop_selectNot
        , testProperty "from-main-headers"     prop_selectFromMainHeaders
        , testProperty "by-header-path/all"    prop_selectByHeaderPathAll
        , testProperty "by-header-path/needle" prop_selectByHeaderPathNeedle
        , testProperty "by-decl-name/all"      prop_selectByDeclNameAll
        , testProperty "by-decl-name/needle"   prop_selectByDeclNameNeedle
        ]
    , testGroup "mergePredicates" [
          testProperty "select/false"     prop_mergeFalse
        , testProperty "select/add/true"  prop_mergeAddTrue
        , testProperty "select/add/false" prop_mergeAddFalse
        , testCase     "true/pos"         mergeTruePos
        , testCase     "true/neg"         mergeTrueNeg
        , testCase     "exclude/one"      mergeExcludeOne
        , testCase     "exclude/two"      mergeExcludeTwo
        ]
    ]

{-------------------------------------------------------------------------------
  Parse pass selection properties
-------------------------------------------------------------------------------}

prop_parseTrue :: SingleLoc -> C.QualPrelimDeclId -> Bool
prop_parseTrue loc qid = matchParse (const True) loc qid PTrue

prop_parseFalse :: SingleLoc -> C.QualPrelimDeclId -> Bool
prop_parseFalse loc qid = not $ matchParse (const True) loc qid PFalse

prop_parseAnd
  :: Fun SingleLoc Bool -> SingleLoc -> C.QualPrelimDeclId -> ParsePredicate
  -> ParsePredicate -> Bool
prop_parseAnd (Fn isMainHeader) loc qid p1 p2 =
    let p1Res = matchParse isMainHeader loc qid p1
        p2Res = matchParse isMainHeader loc qid p2
        p1AndP2Res = matchParse isMainHeader loc qid (PAnd p1 p2)
     in (p1Res && p2Res) == p1AndP2Res

prop_parseOr
  :: Fun SingleLoc Bool -> SingleLoc -> C.QualPrelimDeclId -> ParsePredicate
  -> ParsePredicate -> Bool
prop_parseOr (Fn isMainHeader) loc qid p1 p2 =
    let p1Res = matchParse isMainHeader loc qid p1
        p2Res = matchParse isMainHeader loc qid p2
        p1OrP2Res = matchParse isMainHeader loc qid (POr p1 p2)
     in (p1Res || p2Res) == p1OrP2Res

prop_parseNot
  :: Fun SingleLoc Bool -> SingleLoc -> C.QualPrelimDeclId -> ParsePredicate
  -> Property
prop_parseNot (Fn isMainHeader) loc qid p =
      matchParse isMainHeader loc qid p
  =/= matchParse isMainHeader loc qid (PNot p)

prop_parseSelectFromMainHeaders
  :: Fun SingleLoc Bool -> SingleLoc -> C.QualPrelimDeclId -> Bool
prop_parseSelectFromMainHeaders (Fn isMainHeader) loc qid =
  let p = PIf SelectFromMainHeaders
   in matchParse isMainHeader loc qid p == isMainHeader loc

prop_parseSelectByHeaderPathAll
  :: Fun SingleLoc Bool -> SingleLoc -> C.QualPrelimDeclId -> Bool
prop_parseSelectByHeaderPathAll (Fn isMainHeader) loc qid =
  let p = PIf (SelectByHeaderPath ".*")
   in matchParse isMainHeader loc qid p

prop_parseSelectByHeaderPathNeedle
  :: Fun SingleLoc Bool -> SingleLoc -> C.QualPrelimDeclId -> Bool
prop_parseSelectByHeaderPathNeedle (Fn isMainHeader) loc qid =
  let (SourcePath path) = singleLocPath loc
      path' = path <> "NEEDLE" <> path
      loc' = loc { singleLocPath = SourcePath path' }
      p = PIf (SelectByHeaderPath "NEEDLE")
   in matchParse isMainHeader loc' qid p

{-------------------------------------------------------------------------------
  Select pass selection properties
-------------------------------------------------------------------------------}

prop_selectTrue :: SingleLoc -> C.QualDeclId -> Bool
prop_selectTrue loc qid = matchSelect (const True) loc qid PTrue

prop_selectFalse :: SingleLoc -> C.QualDeclId -> Bool
prop_selectFalse loc qid = not $ matchSelect (const True) loc qid PFalse

prop_selectAnd
  :: Fun SingleLoc Bool -> SingleLoc -> C.QualDeclId -> SelectPredicate
  -> SelectPredicate -> Bool
prop_selectAnd (Fn isMainHeader) loc qid p1 p2 =
    let p1Res = matchSelect isMainHeader loc qid p1
        p2Res = matchSelect isMainHeader loc qid p2
        p1AndP2Res = matchSelect isMainHeader loc qid (PAnd p1 p2)
     in (p1Res && p2Res) == p1AndP2Res

prop_selectOr
  :: Fun SingleLoc Bool -> SingleLoc -> C.QualDeclId -> SelectPredicate
  -> SelectPredicate -> Bool
prop_selectOr (Fn isMainHeader) loc qid p1 p2 =
    let p1Res = matchSelect isMainHeader loc qid p1
        p2Res = matchSelect isMainHeader loc qid p2
        p1OrP2Res = matchSelect isMainHeader loc qid (POr p1 p2)
     in (p1Res || p2Res) == p1OrP2Res

prop_selectNot
  :: Fun SingleLoc Bool -> SingleLoc -> C.QualDeclId -> SelectPredicate
  -> Property
prop_selectNot (Fn isMainHeader) loc qid p =
      matchSelect isMainHeader loc qid p
  =/= matchSelect isMainHeader loc qid (PNot p)

prop_selectFromMainHeaders
  :: Fun SingleLoc Bool -> SingleLoc -> C.QualDeclId -> Bool
prop_selectFromMainHeaders (Fn isMainHeader) loc qid =
  let p = PIf $ Left SelectFromMainHeaders
   in matchSelect isMainHeader loc qid p == isMainHeader loc

prop_selectByHeaderPathAll
  :: Fun SingleLoc Bool -> SingleLoc -> C.QualDeclId -> Bool
prop_selectByHeaderPathAll (Fn isMainHeader) loc qid =
  let p = PIf $ Left (SelectByHeaderPath ".*")
   in matchSelect isMainHeader loc qid p

prop_selectByHeaderPathNeedle
  :: Fun SingleLoc Bool -> SingleLoc -> C.QualDeclId -> Bool
prop_selectByHeaderPathNeedle (Fn isMainHeader) loc qid =
  let (SourcePath path) = singleLocPath loc
      path' = path <> "NEEDLE" <> path
      loc' = loc { singleLocPath = SourcePath path' }
      p = PIf $ Left (SelectByHeaderPath "NEEDLE")
   in matchSelect isMainHeader loc' qid p

prop_selectByDeclNameAll
  :: Fun SingleLoc Bool -> SingleLoc -> C.QualDeclId -> Bool
prop_selectByDeclNameAll (Fn isMainHeader) loc qid =
  let p = PIf $ Right (SelectByDeclName ".*")
   in matchSelect isMainHeader loc qid p

prop_selectByDeclNameNeedle
  :: Fun SingleLoc Bool -> SingleLoc -> C.QualDeclId -> Bool
prop_selectByDeclNameNeedle (Fn isMainHeader) loc qid =
  let name  = C.qualDeclIdName qid
      qid'  = qid { C.qualDeclIdName = name <> "NEEDLE" <> name }
      p     = PIf $ Right (SelectByDeclName "NEEDLE")
   in matchSelect isMainHeader loc qid' p

{-------------------------------------------------------------------------------
  Match tests and properties
-------------------------------------------------------------------------------}

prop_mergeFalse :: [ParsePredicate] -> Property
prop_mergeFalse ps = mergePredicates ps [] === PFalse

prop_mergeAddTrue :: [ParsePredicate] -> [ParsePredicate] -> Property
prop_mergeAddTrue ps qs =
  mergePredicates ps [PTrue] === mergePredicates ps (PTrue : qs)

prop_mergeAddFalse :: [ParsePredicate] -> [ParsePredicate] -> Property
prop_mergeAddFalse ps qs =
  mergePredicates ps qs === mergePredicates (PFalse : ps) qs

mergeTruePos, mergeTrueNeg :: Assertion
mergeTruePos =
  mergePredicates @HeaderPathPredicate []       [PTrue] @?= PTrue
mergeTrueNeg =
  mergePredicates @HeaderPathPredicate [PFalse] [PTrue] @?= PTrue

mergeExcludeOne :: Assertion
mergeExcludeOne = mergePredicates [p] [PTrue] @?= PNot p
  where
    p :: SelectPredicate
    p = PIf $ Right (SelectByDeclName "a")

mergeExcludeTwo :: Assertion
mergeExcludeTwo = mergePredicates [pa, pb] [PTrue] @?= PAnd (PNot pa) (PNot pb)
  where
    pa, pb :: SelectPredicate
    pa = PIf $ Right (SelectByDeclName "a")
    pb = PIf $ Right (SelectByDeclName "b")

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

instance Arbitrary C.QualPrelimDeclId where
  -- TODO: We currently never produce anonymous or builtin declarations.
  -- In this module we check that selection predicates behave as boolean
  -- functions; this is not true for builtins (which are /never/ selected).
  arbitrary = C.QualPrelimDeclIdNamed <$> arbitrary <*> arbitrary

instance Arbitrary C.NameOrigin where
  -- TODO: We currently never produce anonymous or builtin declarations.
  -- See comment for @Arbitrary C.QualPrelimDeclId@
  arbitrary = pure C.NameOriginInSource

instance Arbitrary C.QualDeclId where
  arbitrary = C.QualDeclId <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ParsePredicate where
  arbitrary = oneof [
      pure PTrue
    , PAnd <$> arbitrary <*> arbitrary
    , PNot <$> arbitrary
    , pure (PIf SelectFromMainHeaders)
    , PIf . SelectByHeaderPath <$> elements regexPatterns
    ]

instance Arbitrary SelectPredicate where
  arbitrary = oneof [
      pure PTrue
    , PAnd <$> arbitrary <*> arbitrary
    , PNot <$> arbitrary
    , pure (PIf (Left SelectFromMainHeaders))
    , PIf . Left  . SelectByHeaderPath <$> elements regexPatterns
    , PIf . Right . SelectByDeclName   <$> elements regexPatterns
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
