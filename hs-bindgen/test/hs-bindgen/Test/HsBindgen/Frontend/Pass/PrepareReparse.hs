{-# OPTIONS_GHC -Wno-orphans #-}

module Test.HsBindgen.Frontend.Pass.PrepareReparse (
    tests
  ) where

import Prelude hiding (lex, print)

import Data.Bifunctor (Bifunctor (second))
import Data.Char (isSpace)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Arbitrary1 (..), Property,
                              counterexample, elements, listOf, suchThat,
                              testProperty, (===))
import Text.Parsec (ParseError)

import HsBindgen.Frontend.Pass.PrepareReparse.AST (Decl (..), Tag (..),
                                                   TagName (..), TagType,
                                                   Target (..))
import HsBindgen.Frontend.Pass.PrepareReparse.Lexer (lex)
import HsBindgen.Frontend.Pass.PrepareReparse.Parser (Parse, parse)
import HsBindgen.Frontend.Pass.PrepareReparse.Printer (Print, print)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.HsBindgen.Frontend.Pass.PrepareReparse" [
      testProperty "prop_roundtripCommon @[Target]" $
        prop_roundtripCommon @[Target]
    , testProperty "prop_roundtripCommon @Target" $
        prop_roundtripCommon @Target
    , testProperty "prop_roundtripCommon @Decl" $
        prop_roundtripCommon @Decl
    , testProperty "prop_roundtripCommon @Tag" $
        prop_roundtripCommon @Tag
    , testProperty "prop_roundtripCommon @TagType" $
        prop_roundtripCommon @TagType
    , testProperty "prop_roundtripCommon @TagName" $
        prop_roundtripCommon @TagName
    ]

{-------------------------------------------------------------------------------
  Property
-------------------------------------------------------------------------------}

-- | Test that AST types that are common to 'PreHeader' and 'PostHeader' can
-- rountrip through the printer, lexer, and parser.
--
-- \[
--  \texttt{normalize} \dot \texttt{parse} \dot \texttt{lex} \dot \texttt{print}
--     = \texttt{normalize}
-- \]
--
-- Whitespace is not preserved verbatim, so we normalize the AST types on both
-- sides of the equation.
prop_roundtripCommon ::
     forall a. (Print a, Parse a, Normalize a, Eq a, Show a)
  => a -> Property
prop_roundtripCommon x =
    counterexample ("printed: " <> printed) $
    counterexample ("tokenized: " <> show tokenized) $
    counterexample ("parsed: " <> show parsed) $
    lhs === rhs
  where
    lhs, rhs :: Either ParseError a
    lhs = Right (normalize x)
    rhs = second normalize parsed

    printed = print x ""
    tokenized = lex printed
    parsed = parse =<< tokenized

{-------------------------------------------------------------------------------
  Normalize
-------------------------------------------------------------------------------}

-- When comparing values for equality that involve strings, there is a
-- possibility

class Normalize a where
  normalize :: a -> a

instance Normalize [Target] where
  normalize xs = fmap normalize xs

instance Normalize Target where
  normalize (Target tag decl) = Target (normalize tag) (normalize decl)

instance Normalize Decl where
  normalize (Decl code) = Decl (normalize code)

instance Normalize Tag where
  normalize (Tag typ name) = Tag (normalize typ) (normalize name)

instance Normalize TagType where
  normalize = id

instance Normalize TagName where
  normalize (TagName s) = TagName $ normalize s

instance Normalize String where
  normalize s =
      go $
      map (\x -> if isSpace x then ' ' else x) $
      List.dropWhile isSpace $
      List.dropWhileEnd isSpace $
      s
    where
      go [] = []
      go (x : xs)
        | x == ' ' = x : go (dropWhile (==' ') xs)
        | otherwise = x : go xs

{-------------------------------------------------------------------------------
  Arbitrary
-------------------------------------------------------------------------------}

instance Arbitrary Target where
  arbitrary = Target <$> arbitrary <*> arbitrary
  shrink (Target x y) = uncurry Target <$> shrink (x, y)

instance Arbitrary Decl where
  arbitrary = Decl <$> liftArbitrary (arbitrary `suchThat` (/= '/'))
  shrink (Decl x) = Decl <$> shrink x

instance Arbitrary Tag where
  arbitrary = Tag <$> arbitrary <*> arbitrary
  shrink (Tag x y) = uncurry Tag <$> shrink (x, y)

instance Arbitrary TagName where
  arbitrary = TagName . NE.toList <$> liftArbitrary f
    where
      f = elements $ concat [
            [ 'a' .. 'z' ]
          , [ 'A' .. 'Z' ]
          , [ '0' .. '9' ]
          ]

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary
  shrink (x :| xs) = [
      NE.fromList xs'
    | xs' <- shrink (x:xs)
    , not (null xs')
    ]

instance Arbitrary1 NonEmpty where
  liftArbitrary f = (:|) <$> f <*> listOf f
  liftShrink f (x :| xs) = [
        NE.fromList xs'
      | xs' <- liftShrink f (x:xs)
      , not (null xs')
      ]

instance Arbitrary TagType where
  arbitrary = elements [minBound .. maxBound ]
  shrink _ = [] -- TODO
