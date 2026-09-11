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
import HsBindgen.Frontend.Predicate
import HsBindgen.IR.C qualified as C

tests :: TestTree
tests = testGroup "Test.HsBindgen.Prop.Selection" [
      testGroup "matchSelect" [
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
          testProperty "select/false"     (prop_mergeFalse    @SelectionPredicate)
        , testProperty "select/add/true"  (prop_mergeAddTrue  @SelectionPredicate)
        , testProperty "select/add/false" (prop_mergeAddFalse @SelectionPredicate)
        , testCase     "true/pos"         mergeTruePos
        , testCase     "true/neg"         mergeTrueNeg
        , testCase     "deselect/one"     mergeDeselectOne
        , testCase     "deselect/two"     mergeDeselectTwo
        ]
    ]

{-------------------------------------------------------------------------------
  Select pass selection properties
-------------------------------------------------------------------------------}

prop_selectTrue :: RealPath -> C.DeclName -> C.Availability -> Bool
prop_selectTrue rp name availability =
  matchSelect (const True) (const True) rp name availability BTrue

prop_selectFalse :: RealPath -> C.DeclName -> C.Availability -> Bool
prop_selectFalse rp name availability =
    not $ matchSelect (const True) (const True) rp name availability BFalse

prop_selectAnd
  :: Fun RealPath Bool -> Fun RealPath Bool
  -> RealPath -> C.DeclName -> C.Availability
  -> Boolean SelectionPredicate -> Boolean SelectionPredicate -> Bool
prop_selectAnd (Fn isMainHeader) (Fn isInMainHeaderDir) realPath name availability p1 p2 =
    let p1Res = matchSelect isMainHeader isInMainHeaderDir realPath name availability p1
        p2Res = matchSelect isMainHeader isInMainHeaderDir realPath name availability p2
        p1AndP2Res =
          matchSelect isMainHeader isInMainHeaderDir realPath name availability (BAnd p1 p2)
     in (p1Res && p2Res) == p1AndP2Res

prop_selectOr
  :: Fun RealPath Bool -> Fun RealPath Bool
  -> RealPath -> C.DeclName -> C.Availability
  -> Boolean SelectionPredicate -> Boolean SelectionPredicate -> Bool
prop_selectOr (Fn isMainHeader) (Fn isInMainHeaderDir) realPath name availability p1 p2 =
    let p1Res = matchSelect isMainHeader isInMainHeaderDir realPath name availability p1
        p2Res = matchSelect isMainHeader isInMainHeaderDir realPath name availability p2
        p1OrP2Res =
          matchSelect isMainHeader isInMainHeaderDir realPath name availability (BOr p1 p2)
     in (p1Res || p2Res) == p1OrP2Res

prop_selectNot
  :: Fun RealPath Bool -> Fun RealPath Bool
  -> RealPath -> C.DeclName -> C.Availability
  -> Boolean SelectionPredicate -> Property
prop_selectNot (Fn isMainHeader) (Fn isInMainHeaderDir) realPath name availability p =
      matchSelect isMainHeader isInMainHeaderDir realPath name availability p
  =/= matchSelect isMainHeader isInMainHeaderDir realPath name availability (BNot p)

prop_selectFromMainHeaders
  :: Fun RealPath Bool -> RealPath -> C.DeclName -> C.Availability -> Bool
prop_selectFromMainHeaders (Fn isMainHeader) rp name availability =
  let p = BIf $ SelectHeader FromMainHeaders
   in matchSelect isMainHeader unused rp name availability p == isMainHeader rp

prop_selectFromMainHeaderDirs
  :: Fun RealPath Bool -> RealPath -> C.DeclName -> C.Availability -> Bool
prop_selectFromMainHeaderDirs (Fn isInMainHeaderDir) rp name availability =
  let p = BIf $ SelectHeader FromMainHeaderDirs
   in matchSelect unused isInMainHeaderDir rp name availability p
        == isInMainHeaderDir rp

prop_selectHeaderPathMatchesAll ::
  RealPath -> C.DeclName -> C.Availability -> Bool
prop_selectHeaderPathMatchesAll rp name availability =
  let p = BIf $ SelectHeader (HeaderPathMatches ".*")
   in matchSelect unused unused rp name availability p

prop_selectHeaderPathMatchesNeedle ::
  RealPath -> C.DeclName -> C.Availability -> Bool
prop_selectHeaderPathMatchesNeedle rp name availability =
  let pathT = getRealPathText rp
      rp' = RealPath $ pathT <> "NEEDLE" <> pathT
      p = BIf $ SelectHeader (HeaderPathMatches "NEEDLE")
   in matchSelect unused unused rp' name availability p

prop_selectDeclNameMatchesAll ::
  RealPath -> C.DeclName -> C.Availability -> Bool
prop_selectDeclNameMatchesAll rp name availability =
  let p = BIf $ SelectDecl (DeclNameMatches ".*")
   in matchSelect unused unused rp name availability p

prop_selectDeclNameMatchesNeedle ::
  RealPath -> C.DeclName -> C.Availability -> Bool
prop_selectDeclNameMatchesNeedle rp declName availability =
  let name  = declName.text
      name' = C.DeclName (name <> "NEEDLE" <> name) declName.kind
      p     = BIf $ SelectDecl (DeclNameMatches "NEEDLE")
   in matchSelect unused unused rp name' availability p

prop_selectDeclMatchDeprecated ::
  RealPath -> C.DeclName -> C.Availability -> Bool
prop_selectDeclMatchDeprecated rp name availability =
  let p = BIf $ SelectDecl DeclDeprecated
   in matchSelect unused unused rp name availability p
        == (availability == C.Deprecated)

{-------------------------------------------------------------------------------
  Match tests and properties
-------------------------------------------------------------------------------}

prop_mergeFalse ::
  (Eq a, Show a) => [Boolean a] -> Property
prop_mergeFalse ps = mergeBooleans ps [] === BFalse

prop_mergeAddTrue ::
  (Eq a, Show a) => [Boolean a] -> [Boolean a] -> Property
prop_mergeAddTrue ps qs =
  mergeBooleans ps [BTrue] === mergeBooleans ps (BTrue : qs)

prop_mergeAddFalse ::
  (Eq a, Show a) => [Boolean a] -> [Boolean a] -> Property
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
    p :: Boolean SelectionPredicate
    p = BIf $ SelectDecl (DeclNameMatches "a")

mergeDeselectTwo :: Assertion
mergeDeselectTwo = mergeBooleans [pa, pb] [BTrue] @?= BAnd (BNot pa) (BNot pb)
  where
    pa, pb :: Boolean SelectionPredicate
    pa = BIf $ SelectDecl (DeclNameMatches "a")
    pb = BIf $ SelectDecl (DeclNameMatches "b")

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

instance Arbitrary RealPath where
  arbitrary = RealPath . Text.pack <$> arbitrary

instance Function RealPath where
  function = functionMap
               (Text.unpack . getRealPathText)
               (RealPath . Text.pack)

instance CoArbitrary RealPath where
  coarbitrary = coarbitraryShow

instance Arbitrary C.NameKind where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary C.DeclName where
  arbitrary = C.DeclName <$> (Text.pack <$> arbitrary) <*> arbitrary

instance Arbitrary C.Availability where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary a => Arbitrary (Boolean a) where
  arbitrary = oneof [
      pure BTrue
    , pure BFalse
    , BAnd <$> arbitrary <*> arbitrary
    , BOr <$> arbitrary <*> arbitrary
    , BNot <$> arbitrary
    , BIf <$> arbitrary
    ]

instance Arbitrary SelectionPredicate where
  arbitrary = oneof [
      pure (SelectHeader FromMainHeaders)
    , pure (SelectHeader FromMainHeaderDirs)
    , SelectHeader . HeaderPathMatches <$> elements regexPatterns
    , SelectDecl . DeclNameMatches     <$> elements regexPatterns
    , pure (SelectDecl DeclDeprecated)
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
unused = panicPure "Unexpected use"
