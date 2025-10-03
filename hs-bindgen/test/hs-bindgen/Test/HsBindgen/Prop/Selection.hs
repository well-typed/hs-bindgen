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
        , testCase     "exclude/one"      mergeExcludeOne
        , testCase     "exclude/two"      mergeExcludeTwo
        ]
    ]

{-------------------------------------------------------------------------------
  Parse pass selection properties
-------------------------------------------------------------------------------}

prop_parseTrue :: SourcePath -> Bool
prop_parseTrue path = matchParse (const True) (const True) path PTrue

prop_parseFalse :: SourcePath -> Bool
prop_parseFalse path = not $ matchParse (const True) (const True) path PFalse

prop_parseAnd
  :: Fun SourcePath Bool -> Fun SourcePath Bool -> SourcePath
  -> Boolean ParsePredicate -> Boolean ParsePredicate -> Bool
prop_parseAnd (Fn isMainHeader) (Fn isInMainHeaderDir) path p1 p2 =
    let p1Res = matchParse isMainHeader isInMainHeaderDir path p1
        p2Res = matchParse isMainHeader isInMainHeaderDir path p2
        p1AndP2Res = matchParse isMainHeader isInMainHeaderDir path (PAnd p1 p2)
     in (p1Res && p2Res) == p1AndP2Res

prop_parseOr
  :: Fun SourcePath Bool -> Fun SourcePath Bool -> SourcePath
  -> Boolean ParsePredicate -> Boolean ParsePredicate -> Bool
prop_parseOr (Fn isMainHeader) (Fn isInMainHeaderDir) path p1 p2 =
    let p1Res = matchParse isMainHeader isInMainHeaderDir path p1
        p2Res = matchParse isMainHeader isInMainHeaderDir path p2
        p1OrP2Res = matchParse isMainHeader isInMainHeaderDir path (POr p1 p2)
     in (p1Res || p2Res) == p1OrP2Res

prop_parseNot
  :: Fun SourcePath Bool -> Fun SourcePath Bool -> SourcePath
  -> Boolean ParsePredicate -> Property
prop_parseNot (Fn isMainHeader) (Fn isInMainHeaderDir) path p =
      matchParse isMainHeader isInMainHeaderDir path p
  =/= matchParse isMainHeader isInMainHeaderDir path (PNot p)

prop_parseFromMainHeaders :: Fun SourcePath Bool -> SourcePath -> Bool
prop_parseFromMainHeaders (Fn isMainHeader) path =
  let p = PIf (ParseHeader FromMainHeaders)
   in matchParse isMainHeader unused path p == isMainHeader path

prop_parseFromMainHeaderDirs :: Fun SourcePath Bool -> SourcePath -> Bool
prop_parseFromMainHeaderDirs (Fn isInMainHeaderDir) path =
  let p = PIf (ParseHeader FromMainHeaderDirs)
   in matchParse unused isInMainHeaderDir path p == isInMainHeaderDir path

prop_parseHeaderPathMatchesAll :: SourcePath -> Bool
prop_parseHeaderPathMatchesAll path =
  let p = PIf (ParseHeader (HeaderPathMatches ".*"))
   in matchParse unused unused path p

prop_parseHeaderPathMatchesNeedle :: SourcePath -> Bool
prop_parseHeaderPathMatchesNeedle (SourcePath pathT) =
  let path = SourcePath $ pathT <> "NEEDLE" <> pathT
      p = PIf (ParseHeader (HeaderPathMatches "NEEDLE"))
   in matchParse unused unused path p

{-------------------------------------------------------------------------------
  Select pass selection properties
-------------------------------------------------------------------------------}

prop_selectTrue :: SourcePath -> C.QualDeclId -> C.Availability -> Bool
prop_selectTrue path qid availability =
  matchSelect (const True) (const True) path qid availability PTrue

prop_selectFalse :: SourcePath -> C.QualDeclId -> C.Availability -> Bool
prop_selectFalse path qid availability =
    not $ matchSelect (const True) (const True) path qid availability PFalse

prop_selectAnd
  :: Fun SourcePath Bool -> Fun SourcePath Bool
  -> SourcePath -> C.QualDeclId -> C.Availability
  -> Boolean SelectPredicate -> Boolean SelectPredicate -> Bool
prop_selectAnd (Fn isMainHeader) (Fn isInMainHeaderDir) path qid availability p1 p2 =
    let p1Res = matchSelect isMainHeader isInMainHeaderDir path qid availability p1
        p2Res = matchSelect isMainHeader isInMainHeaderDir path qid availability p2
        p1AndP2Res =
          matchSelect isMainHeader isInMainHeaderDir path qid availability (PAnd p1 p2)
     in (p1Res && p2Res) == p1AndP2Res

prop_selectOr
  :: Fun SourcePath Bool -> Fun SourcePath Bool
  -> SourcePath -> C.QualDeclId -> C.Availability
  -> Boolean SelectPredicate -> Boolean SelectPredicate -> Bool
prop_selectOr (Fn isMainHeader) (Fn isInMainHeaderDir) path qid availability p1 p2 =
    let p1Res = matchSelect isMainHeader isInMainHeaderDir path qid availability p1
        p2Res = matchSelect isMainHeader isInMainHeaderDir path qid availability p2
        p1OrP2Res =
          matchSelect isMainHeader isInMainHeaderDir path qid availability (POr p1 p2)
     in (p1Res || p2Res) == p1OrP2Res

prop_selectNot
  :: Fun SourcePath Bool -> Fun SourcePath Bool
  -> SourcePath -> C.QualDeclId -> C.Availability
  -> Boolean SelectPredicate -> Property
prop_selectNot (Fn isMainHeader) (Fn isInMainHeaderDir) path qid availability p =
      matchSelect isMainHeader isInMainHeaderDir path qid availability p
  =/= matchSelect isMainHeader isInMainHeaderDir path qid availability (PNot p)

prop_selectFromMainHeaders
  :: Fun SourcePath Bool -> SourcePath -> C.QualDeclId -> C.Availability -> Bool
prop_selectFromMainHeaders (Fn isMainHeader) path qid availability =
  let p = PIf $ SelectHeader FromMainHeaders
   in matchSelect isMainHeader unused path qid availability p == isMainHeader path

prop_selectFromMainHeaderDirs
  :: Fun SourcePath Bool -> SourcePath -> C.QualDeclId -> C.Availability -> Bool
prop_selectFromMainHeaderDirs (Fn isInMainHeaderDir) path qid availability =
  let p = PIf $ SelectHeader FromMainHeaderDirs
   in matchSelect unused isInMainHeaderDir path qid availability p
        == isInMainHeaderDir path

prop_selectHeaderPathMatchesAll ::
  SourcePath -> C.QualDeclId -> C.Availability -> Bool
prop_selectHeaderPathMatchesAll path qid availability =
  let p = PIf $ SelectHeader (HeaderPathMatches ".*")
   in matchSelect unused unused path qid availability p

prop_selectHeaderPathMatchesNeedle ::
  SourcePath -> C.QualDeclId -> C.Availability -> Bool
prop_selectHeaderPathMatchesNeedle (SourcePath pathT) qid availability =
  let path = SourcePath $ pathT <> "NEEDLE" <> pathT
      p = PIf $ SelectHeader (HeaderPathMatches "NEEDLE")
   in matchSelect unused unused path qid availability p

prop_selectDeclNameMatchesAll ::
  SourcePath -> C.QualDeclId -> C.Availability -> Bool
prop_selectDeclNameMatchesAll path qid availability =
  let p = PIf $ SelectDecl (DeclNameMatches ".*")
   in matchSelect unused unused path qid availability p

prop_selectDeclNameMatchesNeedle ::
  SourcePath -> C.QualDeclId -> C.Availability -> Bool
prop_selectDeclNameMatchesNeedle path qid availability =
  let name  = C.qualDeclIdName qid
      qid'  = qid { C.qualDeclIdName = name <> "NEEDLE" <> name }
      p     = PIf $ SelectDecl (DeclNameMatches "NEEDLE")
   in matchSelect unused unused path qid' availability p

prop_selectDeclMatchDeprecated ::
  SourcePath -> C.QualDeclId -> C.Availability -> Bool
prop_selectDeclMatchDeprecated path qid availability =
  let p = PIf $ SelectDecl DeclDeprecated
   in matchSelect unused unused path qid availability p
        == (availability == C.Deprecated)

{-------------------------------------------------------------------------------
  Match tests and properties
-------------------------------------------------------------------------------}

prop_mergeFalse :: [Boolean ParsePredicate] -> Property
prop_mergeFalse ps = mergeBooleans ps [] === PFalse

prop_mergeAddTrue ::
  [Boolean ParsePredicate] -> [Boolean ParsePredicate] -> Property
prop_mergeAddTrue ps qs =
  mergeBooleans ps [PTrue] === mergeBooleans ps (PTrue : qs)

prop_mergeAddFalse ::
  [Boolean ParsePredicate] -> [Boolean ParsePredicate] -> Property
prop_mergeAddFalse ps qs =
  mergeBooleans ps qs === mergeBooleans (PFalse : ps) qs

mergeTruePos, mergeTrueNeg :: Assertion
mergeTruePos =
  mergeBooleans @HeaderPathPredicate []       [PTrue] @?= PTrue
mergeTrueNeg =
  mergeBooleans @HeaderPathPredicate [PFalse] [PTrue] @?= PTrue

mergeExcludeOne :: Assertion
mergeExcludeOne = mergeBooleans [p] [PTrue] @?= PNot p
  where
    p :: Boolean SelectPredicate
    p = PIf $ SelectDecl (DeclNameMatches "a")

mergeExcludeTwo :: Assertion
mergeExcludeTwo = mergeBooleans [pa, pb] [PTrue] @?= PAnd (PNot pa) (PNot pb)
  where
    pa, pb :: Boolean SelectPredicate
    pa = PIf $ SelectDecl (DeclNameMatches "a")
    pb = PIf $ SelectDecl (DeclNameMatches "b")

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

instance Arbitrary C.NameOrigin where
  -- TODO: We currently never produce anonymous or builtin declarations.
  -- In this module we check that selection predicates behave as boolean
  -- functions; this is not true for builtins (which are /never/ selected).
  arbitrary = pure C.NameOriginInSource

instance Arbitrary C.QualDeclId where
  arbitrary = C.QualDeclId <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary C.Availability where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary (Boolean ParsePredicate) where
  arbitrary = oneof [
      pure PTrue
    , PAnd <$> arbitrary <*> arbitrary
    , PNot <$> arbitrary
    , pure (PIf (ParseHeader FromMainHeaders))
    , pure (PIf (ParseHeader FromMainHeaderDirs))
    , PIf . ParseHeader . HeaderPathMatches <$> elements regexPatterns
    ]

instance Arbitrary (Boolean SelectPredicate) where
  arbitrary = oneof [
      pure PTrue
    , PAnd <$> arbitrary <*> arbitrary
    , PNot <$> arbitrary
    , pure (PIf (SelectHeader FromMainHeaders))
    , pure (PIf (SelectHeader FromMainHeaderDirs))
    , PIf . SelectHeader  . HeaderPathMatches <$> elements regexPatterns
    , PIf . SelectDecl    . DeclNameMatches   <$> elements regexPatterns
    , pure (PIf (SelectDecl DeclDeprecated))
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
