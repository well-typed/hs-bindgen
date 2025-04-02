{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Internal.QuickCheck.Orphans () where

import Control.Monad (liftM2)
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.ICU.Char as ICU
import Test.QuickCheck

import Clang.CNameSpelling
import Clang.Paths
import HsBindgen.C.AST qualified as C
import HsBindgen.ExtBindings
import HsBindgen.Imports

import Test.Internal.QuickCheck

{-------------------------------------------------------------------------------
  clang Clang.CNameSpelling
-------------------------------------------------------------------------------}

instance Arbitrary CNameSpelling where
  arbitrary = do
    mPrefix <- elements [Just "struct ", Just "enum ", Just "union ", Nothing]
    cname <- C.getCName <$> arbitrary
    return $ CNameSpelling (fromMaybe "" mPrefix <> cname)

{-------------------------------------------------------------------------------
  clang Clang.Paths
-------------------------------------------------------------------------------}

instance Arbitrary SourcePath where
  arbitrary =
    SourcePath . Text.pack . ("/usr/include/" ++) . getCHeaderIncludePath
      <$> arbitrary

instance Arbitrary CHeaderIncludePath where
  arbitrary = do
      path <- (++ ".h") . List.intercalate "/" <$> vectorOfS 1 3 genPart
      construct <- elements [CHeaderSystemIncludePath, CHeaderQuoteIncludePath]
      return $ construct path
    where
      genPart :: Gen String
      genPart = vectorOfS 1 10 $ elements charset

      charset :: String
      charset = "-._~" ++ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']

{-------------------------------------------------------------------------------
  hs-bindgen:internal HsBindgen.C.AST.Name
-------------------------------------------------------------------------------}

instance Arbitrary C.CName where
  arbitrary = frequency [
        (14, aux genStartCharAscii   genContCharAscii)
      , (5,  aux genStartCharUnicode genContCharUnicode)
      , (1,  elements reservedCNames)
      ]
    where
      aux :: Gen Char -> Gen Char -> Gen C.CName
      aux genStartChar genContChar = do
        let genPart = liftM2 (<>) genUnderscores (genContPart genContChar)
        tL <- frequency [
            (9, genStartPart genStartChar genContChar)
          , (1, genPart)
          ]
        tC <- concat <$> vectorOfF [(13, 0), (4, 1), (2, 2), (1, 3)] genPart
        tR <- frequency [(9, pure ""), (1, genUnderscores)]
        return . C.CName $ Text.pack (tL ++ tC ++ tR)

      genStartPart :: Gen Char -> Gen Char -> Gen String
      genStartPart genStartChar genContChar = do
        cL <- genStartChar
        sR <- vectorOfS 0 5 genContChar
        return $ cL : sR

      genContPart :: Gen Char -> Gen String
      genContPart = vectorOfS 1 6

      genUnderscores :: Gen String
      genUnderscores = frequency_ [(16, "_"), (3, "__"), (1, "___")]

      genStartCharAscii, genContCharAscii :: Gen Char
      genStartCharAscii = elements startCharset
      genContCharAscii  = elements contCharset

      startCharset, contCharset :: String
      startCharset = ['A'..'Z'] ++ ['a'..'z']
      contCharset  = ['0'..'9'] ++ startCharset

      genStartCharUnicode, genContCharUnicode :: Gen Char
      genStartCharUnicode =
        frequency [
            (19, arbitraryUnicodeChar `suchThat` isXidStartLetter)
          , (1, arbitraryUnicodeChar `suchThat` isXidStartNotLetter)
          ]
      genContCharUnicode =
        frequency [
            (19, arbitraryUnicodeChar `suchThat` isXidContAlphaNum)
          , (1, arbitraryUnicodeChar `suchThat` isXidContNotAlphaNum)
          ]

      isXidStartLetter, isXidStartNotLetter :: Char -> Bool
      isXidStartLetter =
        liftA2 (&&) (ICU.property ICU.XidStart) Char.isLetter
      isXidStartNotLetter =
        liftA2 (&&) (ICU.property ICU.XidStart) (not . Char.isLetter)

      isXidContAlphaNum, isXidContNotAlphaNum :: Char -> Bool
      isXidContAlphaNum =
        liftA2 (&&) (ICU.property ICU.XidContinue) Char.isAlphaNum
      isXidContNotAlphaNum =
        liftA2 (&&) (ICU.property ICU.XidContinue) (not . Char.isAlphaNum)

      reservedCNames :: [C.CName]
      reservedCNames = [
          "data"
        , "default"
        , "family"
        , "group"
        , "instance"
        , "label"
        , "module"
        , "pattern"
        , "role"
        , "type"
        ]

{-------------------------------------------------------------------------------
  hs-bindgen:internal HsBindgen.ExtBindings
-------------------------------------------------------------------------------}

instance Arbitrary HsPackageName where
  arbitrary =
      HsPackageName . Text.pack . List.intercalate "-"
        <$> vectorOfF [(10, 1), (7, 2), (2, 3), (1, 4)] genPart
    where
      genPart :: Gen String
      genPart =
        vectorOfS 1 6 (elements charset)
          `suchThat` (liftA2 (&&) (not . all Char.isDigit) (/= "all"))

      charset :: String
      charset = filter Char.isAlphaNum ['\0'..'\127']

instance Arbitrary HsModuleName where
  arbitrary =
      HsModuleName . Text.pack . List.intercalate "."
        <$> vectorOfS 1 4 genPart
    where
      genPart :: Gen String
      genPart = do
        cL <- elements startCharset
        sR <- vectorOfS 1 10 $ elements contCharset
        return $ cL : sR

      startCharset, contCharset, ussq :: String
      startCharset = ['A'..'Z']
      contCharset =
        filter (liftA2 (||) Char.isAlphaNum (`elem` ussq)) ['\0'..'\255']
      ussq = "_'"

instance Arbitrary HsIdentifier where
  arbitrary = frequency [
        (7, aux genStartCharAscii   genContCharAscii)
      , (3, aux genStartCharUnicode genContCharUnicode)
      ]
    where
      aux :: Gen Char -> Gen Char -> Gen HsIdentifier
      aux genStartChar genContChar = do
        cL <- genStartChar
        sR <- vectorOfS 0 11 genContChar
        return . HsIdentifier $ Text.pack (cL : sR)

      genStartCharAscii, genContCharAscii :: Gen Char
      genStartCharAscii = elements startCharset
      genContCharAscii  = elements contCharset

      startCharset, contCharset :: String
      startCharset = ['A'..'Z'] ++ ['a'..'z']
      contCharset  = ['0'..'9'] ++ '_' : '\'' : startCharset

      genStartCharUnicode, genContCharUnicode :: Gen Char
      genStartCharUnicode = arbitraryUnicodeChar `suchThat` Char.isLetter
      genContCharUnicode  = arbitraryUnicodeChar `suchThat` isContChar

      isContChar :: Char -> Bool
      isContChar = liftA2 (||) Char.isAlphaNum (`elem` ussq)

      ussq :: String
      ussq = "_'"

instance Arbitrary ExtIdentifier where
  arbitrary = do
    extIdentifierPackage    <- arbitrary
    extIdentifierModule     <- arbitrary
    extIdentifierIdentifier <- arbitrary
    return ExtIdentifier{..}

instance Arbitrary UnresolvedExtBindings where
  arbitrary = do
    unresolvedExtBindingsTypes <- genCNameSpellingMap
    return UnresolvedExtBindings{..}

instance Arbitrary ExtBindings where
  arbitrary = do
    extBindingsTypes <- genCNameSpellingMap
    return ExtBindings{..}

genCNameSpellingMap :: forall a.
     (Arbitrary a, Ord a)
  => Gen (Map CNameSpelling [(Set a, ExtIdentifier)])
genCNameSpellingMap = genMap `suchThat` isValid
  where
    genMap :: Gen (Map CNameSpelling [(Set a, ExtIdentifier)])
    genMap = Map.fromList <$> vectorOfS 0 20 genEntry

    genEntry :: Gen (CNameSpelling, [(Set a, ExtIdentifier)])
    genEntry = do
      cname <- arbitrary
      ps <- vectorOfF [(15, 1), (4, 2), (1, 3)] $ do
        xs <- vectorOfF [(14, 1), (4, 2), (1, 3), (1, 4)] arbitrary
        extId <- arbitrary
        return (Set.fromList xs, extId)
      return (cname, ps)

    isValid :: Map CNameSpelling [(Set a, ExtIdentifier)] -> Bool
    isValid = all (isDisjoint Set.empty . map fst) . Map.elems

    isDisjoint :: Set a -> [Set a] -> Bool
    isDisjoint acc = \case
      s:ss
        | s `Set.disjoint` acc -> isDisjoint (s <> acc) ss
        | otherwise            -> False
      []                       -> True
