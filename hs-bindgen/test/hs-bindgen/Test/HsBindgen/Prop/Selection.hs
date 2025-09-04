{-# OPTIONS_GHC -Wno-orphans #-}

module Test.HsBindgen.Prop.Selection (tests) where

import Data.String (IsString (fromString))
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, HasCallStack, testCase, (@?=))
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), CoArbitrary (coarbitrary),
                              Fun, Function (function),
                              NonNegative (getNonNegative), Property,
                              coarbitraryShow, elements, functionMap, oneof,
                              pattern Fn, testProperty, (=/=), (===))

import Clang.HighLevel.Types
import Clang.Paths
import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Predicate

tests :: TestTree
tests = testGroup "Test.HsBindgen.Prop.Selection" [
      testGroup "matchParse" [
          testProperty "true"                  prop_parseTrue
        , testProperty "false"                 prop_parseFalse
        , testProperty "and"                   prop_parseAnd
        , testProperty "or"                    prop_parseOr
        , testProperty "not"                   prop_parseNot
        , testProperty "from-main-headers"     prop_parseFromMainHeaders
        , testProperty "from-main-header-dirs" prop_parseFromMainHeaderDirs
        , testProperty "header-path/all"       prop_parseHeaderPathMatchesAll
        , testProperty "header-path/needle"    prop_parseHeaderPathMatchesNeedle
        ]
    , testGroup "matchSelect" [
          testProperty "true"                  prop_selectTrue
        , testProperty "false"                 prop_selectFalse
        , testProperty "and"                   prop_selectAnd
        , testProperty "or"                    prop_selectOr
        , testProperty "not"                   prop_selectNot
        , testProperty "from-main-headers"     prop_selectFromMainHeaders
        , testProperty "from-main-header-dirs" prop_selectFromMainHeaderDirs
        , testProperty "header-path/all"       prop_selectHeaderPathMatchesAll
        , testProperty "header-path/needle"    prop_selectHeaderPathMatchesNeedle
        , testProperty "decl-name/all"         prop_selectDeclNameMatchesAll
        , testProperty "decl-name/needle"      prop_selectDeclNameMatchesNeedle
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

prop_parseTrue :: SingleLoc -> C.PrelimDeclId -> Bool
prop_parseTrue loc declId = matchParse (const True) (const True) loc declId PTrue

prop_parseFalse :: SingleLoc -> C.PrelimDeclId -> Bool
prop_parseFalse loc declId =
    not $ matchParse (const True) (const True) loc declId PFalse

prop_parseAnd
  :: Fun SingleLoc Bool -> Fun SingleLoc Bool -> SingleLoc
  -> C.PrelimDeclId -> ParsePredicate -> ParsePredicate -> Bool
prop_parseAnd (Fn isMainHeader) (Fn isInMainHeaderDir) loc declId p1 p2 =
    let p1Res = matchParse isMainHeader isInMainHeaderDir loc declId p1
        p2Res = matchParse isMainHeader isInMainHeaderDir loc declId p2
        p1AndP2Res =
          matchParse isMainHeader isInMainHeaderDir loc declId (PAnd p1 p2)
     in (p1Res && p2Res) == p1AndP2Res

prop_parseOr
  :: Fun SingleLoc Bool -> Fun SingleLoc Bool -> SingleLoc
  -> C.PrelimDeclId -> ParsePredicate -> ParsePredicate -> Bool
prop_parseOr (Fn isMainHeader) (Fn isInMainHeaderDir) loc declId p1 p2 =
    let p1Res = matchParse isMainHeader isInMainHeaderDir loc declId p1
        p2Res = matchParse isMainHeader isInMainHeaderDir loc declId p2
        p1OrP2Res =
          matchParse isMainHeader isInMainHeaderDir loc declId (POr p1 p2)
     in (p1Res || p2Res) == p1OrP2Res

prop_parseNot
  :: Fun SingleLoc Bool -> Fun SingleLoc Bool -> SingleLoc
  -> C.PrelimDeclId -> ParsePredicate -> Property
prop_parseNot (Fn isMainHeader) (Fn isInMainHeaderDir) loc declId p =
      matchParse isMainHeader isInMainHeaderDir loc declId p
  =/= matchParse isMainHeader isInMainHeaderDir loc declId (PNot p)

prop_parseFromMainHeaders
  :: Fun SingleLoc Bool -> SingleLoc -> C.PrelimDeclId -> Bool
prop_parseFromMainHeaders (Fn isMainHeader) loc declId =
  let p = PIf FromMainHeaders
   in matchParse isMainHeader unused loc declId p == isMainHeader loc

prop_parseFromMainHeaderDirs
  :: Fun SingleLoc Bool -> SingleLoc -> C.PrelimDeclId -> Bool
prop_parseFromMainHeaderDirs (Fn isInMainHeaderDir) loc declId =
  let p = PIf FromMainHeaderDirs
   in matchParse unused isInMainHeaderDir loc declId p == isInMainHeaderDir loc

prop_parseHeaderPathMatchesAll :: SingleLoc -> C.PrelimDeclId -> Bool
prop_parseHeaderPathMatchesAll loc declId =
  let p = PIf (HeaderPathMatches ".*")
   in matchParse unused unused loc declId p

prop_parseHeaderPathMatchesNeedle :: SingleLoc -> C.PrelimDeclId -> Bool
prop_parseHeaderPathMatchesNeedle loc declId =
  let (SourcePath path) = singleLocPath loc
      path' = path <> "NEEDLE" <> path
      loc' = loc { singleLocPath = SourcePath path' }
      p = PIf (HeaderPathMatches "NEEDLE")
   in matchParse unused unused loc' declId p

{-------------------------------------------------------------------------------
  Select pass selection properties
-------------------------------------------------------------------------------}

prop_selectTrue :: SingleLoc -> C.QualDeclId -> Bool
prop_selectTrue loc qid = matchSelect (const True) (const True) loc qid PTrue

prop_selectFalse :: SingleLoc -> C.QualDeclId -> Bool
prop_selectFalse loc qid =
    not $ matchSelect (const True) (const True) loc qid PFalse

prop_selectAnd
  :: Fun SingleLoc Bool -> Fun SingleLoc Bool -> SingleLoc -> C.QualDeclId
  -> SelectPredicate -> SelectPredicate -> Bool
prop_selectAnd (Fn isMainHeader) (Fn isInMainHeaderDir) loc qid p1 p2 =
    let p1Res = matchSelect isMainHeader isInMainHeaderDir loc qid p1
        p2Res = matchSelect isMainHeader isInMainHeaderDir loc qid p2
        p1AndP2Res =
          matchSelect isMainHeader isInMainHeaderDir loc qid (PAnd p1 p2)
     in (p1Res && p2Res) == p1AndP2Res

prop_selectOr
  :: Fun SingleLoc Bool -> Fun SingleLoc Bool -> SingleLoc -> C.QualDeclId
  -> SelectPredicate -> SelectPredicate -> Bool
prop_selectOr (Fn isMainHeader) (Fn isInMainHeaderDir) loc qid p1 p2 =
    let p1Res = matchSelect isMainHeader isInMainHeaderDir loc qid p1
        p2Res = matchSelect isMainHeader isInMainHeaderDir loc qid p2
        p1OrP2Res =
          matchSelect isMainHeader isInMainHeaderDir loc qid (POr p1 p2)
     in (p1Res || p2Res) == p1OrP2Res

prop_selectNot
  :: Fun SingleLoc Bool -> Fun SingleLoc Bool -> SingleLoc -> C.QualDeclId
  -> SelectPredicate -> Property
prop_selectNot (Fn isMainHeader) (Fn isInMainHeaderDir) loc qid p =
      matchSelect isMainHeader isInMainHeaderDir loc qid p
  =/= matchSelect isMainHeader isInMainHeaderDir loc qid (PNot p)

prop_selectFromMainHeaders
  :: Fun SingleLoc Bool -> SingleLoc -> C.QualDeclId -> Bool
prop_selectFromMainHeaders (Fn isMainHeader) loc qid =
  let p = PIf $ Left FromMainHeaders
   in matchSelect isMainHeader unused loc qid p == isMainHeader loc

prop_selectFromMainHeaderDirs
  :: Fun SingleLoc Bool -> SingleLoc -> C.QualDeclId -> Bool
prop_selectFromMainHeaderDirs (Fn isInMainHeaderDir) loc qid =
  let p = PIf $ Left FromMainHeaderDirs
   in matchSelect unused isInMainHeaderDir loc qid p == isInMainHeaderDir loc

prop_selectHeaderPathMatchesAll :: SingleLoc -> C.QualDeclId -> Bool
prop_selectHeaderPathMatchesAll loc qid =
  let p = PIf $ Left (HeaderPathMatches ".*")
   in matchSelect unused unused loc qid p

prop_selectHeaderPathMatchesNeedle :: SingleLoc -> C.QualDeclId -> Bool
prop_selectHeaderPathMatchesNeedle loc qid =
  let (SourcePath path) = singleLocPath loc
      path' = path <> "NEEDLE" <> path
      loc' = loc { singleLocPath = SourcePath path' }
      p = PIf $ Left (HeaderPathMatches "NEEDLE")
   in matchSelect unused unused loc' qid p

prop_selectDeclNameMatchesAll :: SingleLoc -> C.QualDeclId -> Bool
prop_selectDeclNameMatchesAll loc qid =
  let p = PIf $ Right (DeclNameMatches ".*")
   in matchSelect unused unused loc qid p

prop_selectDeclNameMatchesNeedle :: SingleLoc -> C.QualDeclId -> Bool
prop_selectDeclNameMatchesNeedle loc qid =
  let name  = C.qualDeclIdName qid
      qid'  = qid { C.qualDeclIdName = name <> "NEEDLE" <> name }
      p     = PIf $ Right (DeclNameMatches "NEEDLE")
   in matchSelect unused unused loc qid' p

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
    p = PIf $ Right (DeclNameMatches "a")

mergeExcludeTwo :: Assertion
mergeExcludeTwo = mergePredicates [pa, pb] [PTrue] @?= PAnd (PNot pa) (PNot pb)
  where
    pa, pb :: SelectPredicate
    pa = PIf $ Right (DeclNameMatches "a")
    pb = PIf $ Right (DeclNameMatches "b")

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
    <*> (getNonNegative <$> arbitrary)

instance Function SingleLoc

instance CoArbitrary SingleLoc

instance Arbitrary C.Name where
  arbitrary = C.Name . Text.pack <$> arbitrary

instance Arbitrary C.NameKind where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary C.PrelimDeclId where
  -- TODO: We currently never produce anonymous or builtin declarations.
  -- In this module we check that selection predicates behave as boolean
  -- functions; this is not true for builtins (which are /never/ selected).
  arbitrary = C.PrelimDeclIdNamed <$> arbitrary

instance Arbitrary C.NameOrigin where
  -- TODO: We currently never produce anonymous or builtin declarations.
  -- See comment for @Arbitrary C.PrelimDeclId@
  arbitrary = pure C.NameOriginInSource

instance Arbitrary C.QualDeclId where
  arbitrary = C.QualDeclId <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ParsePredicate where
  arbitrary = oneof [
      pure PTrue
    , PAnd <$> arbitrary <*> arbitrary
    , PNot <$> arbitrary
    , pure (PIf FromMainHeaders)
    , pure (PIf FromMainHeaderDirs)
    , PIf . HeaderPathMatches <$> elements regexPatterns
    ]

instance Arbitrary SelectPredicate where
  arbitrary = oneof [
      pure PTrue
    , PAnd <$> arbitrary <*> arbitrary
    , PNot <$> arbitrary
    , pure (PIf (Left FromMainHeaders))
    , pure (PIf (Left FromMainHeaderDirs))
    , PIf . Left  . HeaderPathMatches <$> elements regexPatterns
    , PIf . Right . DeclNameMatches   <$> elements regexPatterns
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

unused :: HasCallStack => a
unused = panicPure "unexpected use"
