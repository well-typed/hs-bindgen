{-# OPTIONS_GHC -Wno-orphans #-}

module Test.HsBindgen.C.Predicate
  ( tests
  ) where

import Data.Either (isRight)
import Data.String (IsString (fromString))
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), CoArbitrary (coarbitrary),
                              Fun, Function (function),
                              NonNegative (getNonNegative), Property,
                              coarbitraryShow, elements, functionMap, oneof,
                              pattern Fn, testProperty, (=/=))

import Clang.HighLevel.Types
import Clang.Paths
import HsBindgen.C.Predicate
import HsBindgen.Language.C

tests :: TestTree
tests = testGroup "selection-predicate" [
      testProperty "all"                    prop_selectAll
    , testProperty "from-main-file"         prop_selectFromMainFiles
    , testProperty "by-file-name/all"       prop_selectByFileNameAll
    , testProperty "by-file-name/needle"    prop_selectByFileNameNeedle
    , testProperty "by-element-name/all"    prop_selectByElementNameAll
    , testProperty "by-element-name/needle" prop_selectByElementNameNeedle
    , testProperty "negate"                 prop_selectNegate
    , testProperty "both"                   prop_selectIfBoth
    ]

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

prop_selectAll :: SingleLoc -> Maybe QualName -> Bool
prop_selectAll loc name = select (const True) loc name SelectAll

prop_selectIfBoth
  :: Fun SingleLoc Bool -> SingleLoc -> Maybe QualName
  -> Predicate -> Predicate -> Bool
prop_selectIfBoth (Fn isMainFile) loc name p1 p2 =
  let p1Res = select isMainFile loc name p1
      p2Res = select isMainFile loc name p2
      p1AndP2Res = select isMainFile loc name (SelectIfBoth p1 p2)
   in (p1Res && p2Res) == p1AndP2Res

prop_selectNegate
  :: Fun SingleLoc Bool -> SingleLoc -> Maybe QualName -> Predicate -> Property
prop_selectNegate (Fn isMainFile) loc name predicate =
  select isMainFile loc name predicate
  =/= select isMainFile loc name (SelectNegate predicate)

prop_selectFromMainFiles
  :: Fun SingleLoc Bool -> SingleLoc -> Maybe QualName -> Bool
prop_selectFromMainFiles (Fn isMainFile) loc name =
  select isMainFile loc name SelectFromMainFiles == isMainFile loc

prop_selectByFileNameAll
  :: Fun SingleLoc Bool -> SingleLoc -> Maybe QualName -> Bool
prop_selectByFileNameAll (Fn isMainFile) loc name =
  select isMainFile loc name (SelectByFileName ".*")

prop_selectByFileNameNeedle
  :: Fun SingleLoc Bool -> SingleLoc -> Maybe QualName -> Bool
prop_selectByFileNameNeedle (Fn isMainFile) loc name =
  let (SourcePath sourcePath) = singleLocPath loc
      sourcePath' = sourcePath <> "NEEDLE" <> sourcePath
      loc' = loc { singleLocPath = SourcePath sourcePath'}
   in select isMainFile loc' name (SelectByFileName "NEEDLE")

prop_selectByElementNameAll
  :: Fun SingleLoc Bool -> SingleLoc -> Maybe QualName -> Bool
prop_selectByElementNameAll (Fn isMainFile) loc name =
    maybeNot $ select isMainFile loc name (SelectByElementName ".*")
  where
    maybeNot = maybe not (const id) name

prop_selectByElementNameNeedle
  :: Fun SingleLoc Bool -> SingleLoc -> QualName -> Bool
prop_selectByElementNameNeedle (Fn isMainFile) loc name =
  let cname = getCName $ qualNameName name
      cname' = cname <> "NEEDLE" <> cname
      name' = name { qualNameName = CName cname'}
   in select isMainFile loc (Just name') (SelectByElementName "NEEDLE")

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

instance Arbitrary CName where
  arbitrary = CName . Text.pack <$> arbitrary

instance Arbitrary NameKind where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary QualName where
  arbitrary = QualName <$> arbitrary <*> arbitrary

instance Arbitrary Predicate where
  arbitrary = oneof [
                  pure SelectAll
                , SelectIfBoth <$> arbitrary <*> arbitrary
                , SelectNegate <$> arbitrary
                , pure SelectFromMainFiles
                , SelectByFileName <$> elements regexPatterns
                , SelectByElementName <$> elements regexPatterns
                ]

fmapCompose4
  :: (r1 -> r2)
  -> (a1 -> a2 -> a3 -> a4 -> r1)
  -> a1 -> a2 -> a3 -> a4 -> r2
fmapCompose4 f g x1 x2 x3 x4 = f (g x1 x2 x3 x4)

select :: IsMainFile -> SingleLoc -> Maybe QualName -> Predicate -> Bool
select = isRight `fmapCompose4` matchPredicate

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
