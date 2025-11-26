{-# OPTIONS_GHC -Wno-orphans #-}

module Test.HsBindgen.Prop.Selection (tests) where

import Data.String (IsString (fromString))
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, HasCallStack, testCase, (@?=))
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), CoArbitrary (coarbitrary),
                              Fun, Function (function), Property,
                              coarbitraryShow, elements, functionMap, oneof,
                              pattern Fn, testProperty, (=/=), (===))

import Clang.Paths

import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.AST.External qualified as C
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
        , testProperty "decl-deprecated"       prop_selectDeclMatchDeprecated
        ]
    , testGroup "mergeBooleans" [
          testProperty "select/false"     prop_mergeFalse
        , testProperty "select/add/true"  prop_mergeAddTrue
        , testProperty "select/add/false" prop_mergeAddFalse
        , testCase     "true/pos"         mergeTruePos
        , testCase     "true/neg"         mergeTrueNeg
        , testCase     "deselect/one"      mergeDeselectOne
        , testCase     "deselect/two"      mergeDeselectTwo
        ]
    ]

{-------------------------------------------------------------------------------
  Parse pass selection properties
-------------------------------------------------------------------------------}

prop_parseTrue :: SourcePath -> Bool
prop_parseTrue path = matchParse (const True) (const True) path BTrue

prop_parseFalse :: SourcePath -> Bool
prop_parseFalse path = not $ matchParse (const True) (const True) path BFalse

prop_parseAnd
  :: Fun SourcePath Bool -> Fun SourcePath Bool -> SourcePath
  -> Boolean ParsePredicate -> Boolean ParsePredicate -> Bool
prop_parseAnd (Fn isMainHeader) (Fn isInMainHeaderDir) path p1 p2 =
    let p1Res = matchParse isMainHeader isInMainHeaderDir path p1
        p2Res = matchParse isMainHeader isInMainHeaderDir path p2
        p1AndP2Res = matchParse isMainHeader isInMainHeaderDir path (BAnd p1 p2)
     in (p1Res && p2Res) == p1AndP2Res

prop_parseOr
  :: Fun SourcePath Bool -> Fun SourcePath Bool -> SourcePath
  -> Boolean ParsePredicate -> Boolean ParsePredicate -> Bool
prop_parseOr (Fn isMainHeader) (Fn isInMainHeaderDir) path p1 p2 =
    let p1Res = matchParse isMainHeader isInMainHeaderDir path p1
        p2Res = matchParse isMainHeader isInMainHeaderDir path p2
        p1OrP2Res = matchParse isMainHeader isInMainHeaderDir path (BOr p1 p2)
     in (p1Res || p2Res) == p1OrP2Res

prop_parseNot
  :: Fun SourcePath Bool -> Fun SourcePath Bool -> SourcePath
  -> Boolean ParsePredicate -> Property
prop_parseNot (Fn isMainHeader) (Fn isInMainHeaderDir) path p =
      matchParse isMainHeader isInMainHeaderDir path p
  =/= matchParse isMainHeader isInMainHeaderDir path (BNot p)

prop_parseFromMainHeaders :: Fun SourcePath Bool -> SourcePath -> Bool
prop_parseFromMainHeaders (Fn isMainHeader) path =
  let p = BIf (ParseHeader FromMainHeaders)
   in matchParse isMainHeader unused path p == isMainHeader path

prop_parseFromMainHeaderDirs :: Fun SourcePath Bool -> SourcePath -> Bool
prop_parseFromMainHeaderDirs (Fn isInMainHeaderDir) path =
  let p = BIf (ParseHeader FromMainHeaderDirs)
   in matchParse unused isInMainHeaderDir path p == isInMainHeaderDir path

prop_parseHeaderPathMatchesAll :: SourcePath -> Bool
prop_parseHeaderPathMatchesAll path =
  let p = BIf (ParseHeader (HeaderPathMatches ".*"))
   in matchParse unused unused path p

prop_parseHeaderPathMatchesNeedle :: SourcePath -> Bool
prop_parseHeaderPathMatchesNeedle (SourcePath pathT) =
  let path = SourcePath $ pathT <> "NEEDLE" <> pathT
      p = BIf (ParseHeader (HeaderPathMatches "NEEDLE"))
   in matchParse unused unused path p

{-------------------------------------------------------------------------------
  Select pass selection properties
-------------------------------------------------------------------------------}

prop_selectTrue :: SourcePath -> C.QualName -> C.Availability -> Bool
prop_selectTrue path name availability =
  matchSelect (const True) (const True) path name availability BTrue

prop_selectFalse :: SourcePath -> C.QualName -> C.Availability -> Bool
prop_selectFalse path name availability =
    not $ matchSelect (const True) (const True) path name availability BFalse

prop_selectAnd
  :: Fun SourcePath Bool -> Fun SourcePath Bool
  -> SourcePath -> C.QualName -> C.Availability
  -> Boolean SelectPredicate -> Boolean SelectPredicate -> Bool
prop_selectAnd (Fn isMainHeader) (Fn isInMainHeaderDir) path name availability p1 p2 =
    let p1Res = matchSelect isMainHeader isInMainHeaderDir path name availability p1
        p2Res = matchSelect isMainHeader isInMainHeaderDir path name availability p2
        p1AndP2Res =
          matchSelect isMainHeader isInMainHeaderDir path name availability (BAnd p1 p2)
     in (p1Res && p2Res) == p1AndP2Res

prop_selectOr
  :: Fun SourcePath Bool -> Fun SourcePath Bool
  -> SourcePath -> C.QualName -> C.Availability
  -> Boolean SelectPredicate -> Boolean SelectPredicate -> Bool
prop_selectOr (Fn isMainHeader) (Fn isInMainHeaderDir) path name availability p1 p2 =
    let p1Res = matchSelect isMainHeader isInMainHeaderDir path name availability p1
        p2Res = matchSelect isMainHeader isInMainHeaderDir path name availability p2
        p1OrP2Res =
          matchSelect isMainHeader isInMainHeaderDir path name availability (BOr p1 p2)
     in (p1Res || p2Res) == p1OrP2Res

prop_selectNot
  :: Fun SourcePath Bool -> Fun SourcePath Bool
  -> SourcePath -> C.QualName -> C.Availability
  -> Boolean SelectPredicate -> Property
prop_selectNot (Fn isMainHeader) (Fn isInMainHeaderDir) path name availability p =
      matchSelect isMainHeader isInMainHeaderDir path name availability p
  =/= matchSelect isMainHeader isInMainHeaderDir path name availability (BNot p)

prop_selectFromMainHeaders
  :: Fun SourcePath Bool -> SourcePath -> C.QualName -> C.Availability -> Bool
prop_selectFromMainHeaders (Fn isMainHeader) path name availability =
  let p = BIf $ SelectHeader FromMainHeaders
   in matchSelect isMainHeader unused path name availability p == isMainHeader path

prop_selectFromMainHeaderDirs
  :: Fun SourcePath Bool -> SourcePath -> C.QualName -> C.Availability -> Bool
prop_selectFromMainHeaderDirs (Fn isInMainHeaderDir) path name availability =
  let p = BIf $ SelectHeader FromMainHeaderDirs
   in matchSelect unused isInMainHeaderDir path name availability p
        == isInMainHeaderDir path

prop_selectHeaderPathMatchesAll ::
  SourcePath -> C.QualName -> C.Availability -> Bool
prop_selectHeaderPathMatchesAll path name availability =
  let p = BIf $ SelectHeader (HeaderPathMatches ".*")
   in matchSelect unused unused path name availability p

prop_selectHeaderPathMatchesNeedle ::
  SourcePath -> C.QualName -> C.Availability -> Bool
prop_selectHeaderPathMatchesNeedle (SourcePath pathT) name availability =
  let path = SourcePath $ pathT <> "NEEDLE" <> pathT
      p = BIf $ SelectHeader (HeaderPathMatches "NEEDLE")
   in matchSelect unused unused path name availability p

prop_selectDeclNameMatchesAll ::
  SourcePath -> C.QualName -> C.Availability -> Bool
prop_selectDeclNameMatchesAll path name availability =
  let p = BIf $ SelectDecl (DeclNameMatches ".*")
   in matchSelect unused unused path name availability p

prop_selectDeclNameMatchesNeedle ::
  SourcePath -> C.QualName -> C.Availability -> Bool
prop_selectDeclNameMatchesNeedle path qualName availability =
  let name  = C.qualNameName qualName
      name'  = qualName { C.qualNameName = name <> "NEEDLE" <> name }
      p     = BIf $ SelectDecl (DeclNameMatches "NEEDLE")
   in matchSelect unused unused path name' availability p

prop_selectDeclMatchDeprecated ::
  SourcePath -> C.QualName -> C.Availability -> Bool
prop_selectDeclMatchDeprecated path name availability =
  let p = BIf $ SelectDecl DeclDeprecated
   in matchSelect unused unused path name availability p
        == (availability == C.Deprecated)

{-------------------------------------------------------------------------------
  Match tests and properties
-------------------------------------------------------------------------------}

prop_mergeFalse :: [Boolean ParsePredicate] -> Property
prop_mergeFalse ps = mergeBooleans ps [] === BFalse

prop_mergeAddTrue ::
  [Boolean ParsePredicate] -> [Boolean ParsePredicate] -> Property
prop_mergeAddTrue ps qs =
  mergeBooleans ps [BTrue] === mergeBooleans ps (BTrue : qs)

prop_mergeAddFalse ::
  [Boolean ParsePredicate] -> [Boolean ParsePredicate] -> Property
prop_mergeAddFalse ps qs =
  mergeBooleans ps qs === mergeBooleans (BFalse : ps) qs

mergeTruePos, mergeTrueNeg :: Assertion
mergeTruePos =
  mergeBooleans @HeaderPathPredicate []       [BTrue] @?= BTrue
mergeTrueNeg =
  mergeBooleans @HeaderPathPredicate [BFalse] [BTrue] @?= BTrue

mergeDeselectOne :: Assertion
mergeDeselectOne = mergeBooleans [p] [BTrue] @?= BNot p
  where
    p :: Boolean SelectPredicate
    p = BIf $ SelectDecl (DeclNameMatches "a")

mergeDeselectTwo :: Assertion
mergeDeselectTwo = mergeBooleans [pa, pb] [BTrue] @?= BAnd (BNot pa) (BNot pb)
  where
    pa, pb :: Boolean SelectPredicate
    pa = BIf $ SelectDecl (DeclNameMatches "a")
    pb = BIf $ SelectDecl (DeclNameMatches "b")

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

instance Arbitrary C.Name where
  arbitrary = C.Name . Text.pack <$> arbitrary

instance Arbitrary C.NameKind where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary C.QualName where
  arbitrary = C.QualName <$> arbitrary <*> arbitrary

instance Arbitrary C.Availability where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary (Boolean ParsePredicate) where
  arbitrary = oneof [
      pure BTrue
    , BAnd <$> arbitrary <*> arbitrary
    , BNot <$> arbitrary
    , pure (BIf (ParseHeader FromMainHeaders))
    , pure (BIf (ParseHeader FromMainHeaderDirs))
    , BIf . ParseHeader . HeaderPathMatches <$> elements regexPatterns
    ]

instance Arbitrary (Boolean SelectPredicate) where
  arbitrary = oneof [
      pure BTrue
    , BAnd <$> arbitrary <*> arbitrary
    , BNot <$> arbitrary
    , pure (BIf (SelectHeader FromMainHeaders))
    , pure (BIf (SelectHeader FromMainHeaderDirs))
    , BIf . SelectHeader  . HeaderPathMatches <$> elements regexPatterns
    , BIf . SelectDecl    . DeclNameMatches   <$> elements regexPatterns
    , pure (BIf (SelectDecl DeclDeprecated))
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
