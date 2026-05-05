{-# LANGUAGE RoleAnnotations #-}

module Test.HsBindgen.Frontend.LanguageC (
    tests
    -- * Properties (exported for haddocks)
  , prop_reparseGlobal_type_roundtripCanonical
  , prop_reparseGlobal_primType_roundtripCanonical
  , prop_reparseGlobal_complexType_roundtripCanonical
  , prop_reparseGlobal_type_nonCanonicalToCanonical
  , prop_reparseGlobal_primType_nonCanonicalToCanonical
  , prop_reparseGlobal_complexType_nonCanonicalToCanonical
  , prop_reparseGlobal_longDouble
  ) where

import Control.Exception (Exception, throwIO)
import Data.Bifunctor (Bifunctor (first))
import Data.Default (Default (def))
import Data.List qualified as List
import Data.Map.Lazy qualified as Map
import Data.Text qualified as Text
import GHC.Show (appPrec, showSpace)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), Property, conjoin,
                              counterexample, elements, ioProperty, once, oneof,
                              tabulate, testProperty, (===))

import Clang.Args (ClangArgs)
import Clang.Enum.Bitfield (BitfieldEnum, bitfieldEnum)
import Clang.Enum.Simple (SimpleEnum)
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types (Diagnostic, MultiLoc (multiLocExpansion),
                              Range (rangeStart), Token (tokenExtent),
                              TokenSpelling, diagnosticIsError)
import Clang.LowLevel.Core (CXErrorCode, CXIndex, CXTranslationUnit,
                            CXTranslationUnit_Flags (CXTranslationUnit_DetailedPreprocessingRecord),
                            CXUnsavedFile,
                            DisplayDiagnostics (DontDisplayDiagnostics),
                            clang_getTranslationUnitCursor)
import Clang.Paths (SourcePath (SourcePath))

import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.PrettyPrinter (showsVariableType)
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.LanguageC qualified as LanC
import HsBindgen.Frontend.Pass.PrepareReparse.Flatten (flattenDefault)
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass (FlatTokens (FlatTokens, flatten, locStart),
                                                      PrepareReparse)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass (ReparseMacroExpansions)
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.HsBindgen.Frontend.LanguageC" [
      testProperty "prop_reparseGlobal_type_roundtripCanonical"
        prop_reparseGlobal_type_roundtripCanonical
    , testProperty "prop_reparseGlobal_primType_roundtripCanonical"
        prop_reparseGlobal_primType_roundtripCanonical
    , testProperty "prop_reparseGlobal_complexType_roundtripCanonical"
        prop_reparseGlobal_complexType_roundtripCanonical
    , testProperty "prop_reparseGlobal_type_nonCanonicalToCanonical"
        prop_reparseGlobal_type_nonCanonicalToCanonical
    , testProperty "prop_reparseGlobal_primType_nonCanonicalToCanonical"
        prop_reparseGlobal_primType_nonCanonicalToCanonical
    , testProperty "prop_reparseGlobal_complexType_nonCanonicalToCanonical"
        prop_reparseGlobal_complexType_nonCanonicalToCanonical
    , testProperty "prop_reparseGlobal_longDouble"
        prop_reparseGlobal_longDouble
    ]

{-------------------------------------------------------------------------------
  Properties: canonical types
-------------------------------------------------------------------------------}

-- |
--
-- 'prop_reparseGlobal_type_roundtripCanonical' \(\tau\) is a property that
-- checks that a canonical C type \(\tau\) that we put into the reparser is the
-- C type that we get out of the reparser. In other words, it should
-- /roundtrip/:
--
-- \[
--  \forall \tau. \text{reparseGlobal} ~ (\tau ~ \texttt{x;}) = \tau
-- \]
--
prop_reparseGlobal_type_roundtripCanonical :: CanonicalType -> Property
prop_reparseGlobal_type_roundtripCanonical ty =
    prop_reparseGlobal
      (\var -> showsVariableType (showString var) ty.unwrap)
      (Right (coercePass ty.unwrap))

-- | Variant of 'prop_reparseGlobal_type_roundtripCanonical' for /primitive/ C
-- types
prop_reparseGlobal_primType_roundtripCanonical :: CanonicalPrimType -> Property
prop_reparseGlobal_primType_roundtripCanonical ty =
    prop_reparseGlobal_type_roundtripCanonical $
      CanonicalType (C.TypePrim ty.unwrap)

-- | Variant of 'prop_reparseGlobal_type_roundtripCanonical' for /complex/ C
-- types
prop_reparseGlobal_complexType_roundtripCanonical :: CanonicalComplexType -> Property
prop_reparseGlobal_complexType_roundtripCanonical ty =
    prop_reparseGlobal_type_roundtripCanonical $
      CanonicalType (C.TypeComplex (C.PrimFloating ty.unwrap))

{-------------------------------------------------------------------------------
  Canonical types
-------------------------------------------------------------------------------}

-- | A C type can have multiple textual representations, but only one is
-- canonical. When we put any representation into the reparser, we expect to get
-- the canonical representation out.
--
-- Each canonical representation corresponds to a unqiue value of our AST for C
-- types.
newtype CanonicalType = CanonicalType { unwrap :: C.Type PrePass }
  deriving stock (Show, Eq)

instance Arbitrary CanonicalType where
  -- TODO: make this truly arbitrary
  arbitrary = CanonicalType <$> oneof [
      elements [
          C.TypeQual C.QualConst (C.TypePointers 2 (C.TypePrim (C.PrimFloating C.PrimDouble)))
        , C.TypePointers 1 (C.TypeIncompleteArray (C.TypePrim (C.PrimChar (C.PrimSignImplicit Nothing))))
        ]
    ]

-- | A primitive C type can have multiple textual representations, but only one
-- is canonical. When we put any representation into the reparser, we expect to
-- get the canonical representation out.
--
-- Each canonical representation corresponds to a unqiue value of our AST for
-- primitive C types.
newtype CanonicalPrimType = CanonicalPrimType { unwrap :: C.PrimType }
  deriving stock (Show, Eq)

instance Arbitrary CanonicalPrimType where
  arbitrary = elements enumerate

-- | A complex C type can have multiple textual representations, but only one is
-- canonical. When we put any representation into the reparser, we expect to get
-- the canonical representation out.
--
-- Each canonical representation corresponds to a unqiue value of our AST for
-- complex C types.
newtype CanonicalComplexType = CanonicalComplexType { unwrap :: C.PrimFloatType }
  deriving stock (Show, Eq)

instance Arbitrary CanonicalComplexType where
  arbitrary = elements enumerate

{-------------------------------------------------------------------------------
  Properties: non-canonical types
-------------------------------------------------------------------------------}

-- |
--
-- 'prop_reparseGlobal_type_nonCanonicalToCanonical' \(\tau\) is a property that
-- checks that for a non-canonical C type \(\tau\) that we put into the reparser
-- we get out a C type \(\sigma\) that is the canonical representation of
-- \(\tau\).
--
-- \[
--  \forall \tau, \sigma.
--    isCanonical(\tau, \sigma) \implies
--      \text{reparseGlobal} ~ (\tau ~ \texttt{x;}) = \sigma
-- \]
--
prop_reparseGlobal_type_nonCanonicalToCanonical :: NonCanonicalType -> Property
prop_reparseGlobal_type_nonCanonicalToCanonical ty =
    prop_reparseGlobal
      ty.representation
      (Right (coercePass ty.canonical))

-- | Variant of 'prop_reparseGlobal_type_nonCanonicalToCanonical' for
-- /primitive/ C types
prop_reparseGlobal_primType_nonCanonicalToCanonical :: NonCanonicalPrimType -> Property
prop_reparseGlobal_primType_nonCanonicalToCanonical ty =
    prop_reparseGlobal_type_nonCanonicalToCanonical $
      NonCanonicalType {
          representation = \var -> showString ty.representation . showSpace . showString var
        , canonical = C.TypePrim ty.canonical
        }

-- | Variant of 'prop_reparseGlobal_type_nonCanonicalToCanonical' for /complex/
-- C types
prop_reparseGlobal_complexType_nonCanonicalToCanonical :: NonCanonicalComplexType -> Property
prop_reparseGlobal_complexType_nonCanonicalToCanonical ty =
    prop_reparseGlobal_type_nonCanonicalToCanonical $
      NonCanonicalType {
          representation = \var -> showString ty.representation . showSpace . showString var
        , canonical = ty.canonical
        }

{-------------------------------------------------------------------------------
  (Non-)canonical types
-------------------------------------------------------------------------------}

-- | A C type that is not guaranteed to be a a canonical type. See
-- 'CanonicalType'.
data NonCanonicalType = NonCanonicalType {
    -- | A function of variable name to string
    --
    -- Some C types (like arrays) require printing characters before and after a
    -- variable name. For example, an array-of-pointer-to-int:
    --
    -- > int (*x)[]
    --
    -- Hence, 'representation' is a function of variable name to string.
    representation :: String -> ShowS
  , canonical      :: C.Type PrePass
  }

instance Show NonCanonicalType where
  showsPrec d ty = showParen (d > appPrec) $
      showString "NonCanonicalType " . ty.representation ""

instance Arbitrary NonCanonicalType where
  -- TODO: make this truly arbitrary
  arbitrary = elements [
        NonCanonicalType {
            representation = \var -> showString "_Bool " . showString var . showString " [3]"
          , canonical = C.TypeConstArray 3 (C.TypePrim C.PrimBool)
          }
      ]

-- | A primitive C type that is not guaranteed to be a a canonical type. See
-- 'CanonicalPrimType'.
data NonCanonicalPrimType = NonCanonicalPrimType {
    representation :: String
  , canonical      :: C.PrimType
  }
  deriving stock (Show, Eq)

instance Arbitrary NonCanonicalPrimType where
  arbitrary = elements enumerate

-- | A complex C type that is not guaranteed to be a a canonical type. See
-- 'CanonicalComplexType'.
data NonCanonicalComplexType = NonCanonicalComplexType {
    representation :: String
  , canonical      :: C.Type PrePass
  }
  deriving stock (Show, Eq)

instance Arbitrary NonCanonicalComplexType where
  arbitrary = elements enumerate

{-------------------------------------------------------------------------------
  Properties: unsupported types
-------------------------------------------------------------------------------}

-- | Check that @long doubl@ and @long double _Complex@ are not supported
prop_reparseGlobal_longDouble :: Property
prop_reparseGlobal_longDouble = once $ conjoin [
      prop_reparseGlobal (mkInput "long double") (Left (UpdateUnsupported "long double"))
    , prop_reparseGlobal (mkInput "double long") (Left (UpdateUnsupported "long double"))
    , prop_reparseGlobal (mkInput "long double _Complex") (Left (UpdateUnsupported "long double"))
    , prop_reparseGlobal (mkInput "double long _Complex") (Left (UpdateUnsupported "long double"))
    , prop_reparseGlobal (mkInput "long _Complex double") (Left (UpdateUnsupported "long double"))
    , prop_reparseGlobal (mkInput "double _Complex long") (Left (UpdateUnsupported "long double"))
    , prop_reparseGlobal (mkInput "_Complex long double") (Left (UpdateUnsupported "long double"))
    , prop_reparseGlobal (mkInput "_Complex double long") (Left (UpdateUnsupported "long double"))
    ]
  where
    mkInput typ = \var -> showString typ . showSpace . showString var

{-------------------------------------------------------------------------------
  Properties: basic
-------------------------------------------------------------------------------}

-- | Run the reparser and check that it has the expected output.
prop_reparseGlobal ::
     -- | Input: function of variable name to a string representing the type
     -- applied to that variable.
     (String -> ShowS)
     -- | Expected output: either an error, or a reparsed type
  -> Either Error (C.Type ReparseMacroExpansions)
  -> Property
prop_reparseGlobal input expectedOutput =
    ioProperty $ do
      tokens <- tokenize contents
      let flatTokens = FlatTokens {
              flatten = flattenDefault tokens
            , locStart = getLocation tokens
            }
      let output = LanC.reparseGlobal Map.empty flatTokens
      pure $
        counterexample ("reparser input: " <> contents) $
        tabulate "reparser input" [contents] $
        expectedOutput === first removeCallstack output
  where
    contents = input "x" ";"

    removeCallstack = \case
        LanC.UpdateUnexpected _ str -> UpdateUnexpected str
        LanC.UpdateUnsupported str  -> UpdateUnsupported str

    getLocation :: [Token a] -> MultiLoc
    getLocation []    = panicPure "Unexpected empty list of tokens"
    getLocation (t:_) = t.tokenExtent.rangeStart

-- | The pass that comes just before the 'ReparseMacroExpansions' pass
type PrePass = PrepareReparse

-- | Copy of 'LanC.Error' without the callstack
--
-- We want to use an 'Eq' instance for 'LanC.Error' but it has a 'CallStack'
-- field, and 'CallStack' does not have an 'Eq' instance. So we define our own
-- version of 'LanC.Error' without the callstack.
data Error =
    UpdateUnexpected String
  | UpdateUnsupported String
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Enumerate
-------------------------------------------------------------------------------}

-- | Types for which all possible values can be enumerated
class Enumerate a where
  -- | Enumerate all possible values of a type
  --
  -- We advise to only define 'Enumerate' instances for types that are finite
  -- (so that 'enumerate' can finish computing), and small(-ish) (to avoid
  -- rampant combinatorial explosion).
  enumerate :: [a]
  default enumerate :: (Enum a, Bounded a) => [a]
  enumerate = [minBound .. maxBound]

instance Enumerate a => Enumerate (Maybe a) where
  enumerate = concat [
        pure Nothing
      , Just <$> enumerate
      ]

{-------------------------------------------------------------------------------
  Enumerate C types
-------------------------------------------------------------------------------}

instance Enumerate C.PrimType where
  enumerate = concat [
        C.PrimChar <$> enumerate
      , C.PrimIntegral <$> enumerate <*> enumerate
      , C.PrimFloating <$> enumerate
      , pure C.PrimBool
      ]

instance Enumerate C.PrimIntType
instance Enumerate C.PrimFloatType
instance Enumerate C.PrimSign

instance Enumerate C.PrimSignChar where
  enumerate = concat [
        C.PrimSignExplicit <$> enumerate
        -- NOTE: we do not enumerate the @Just@ cases because they are printed
        -- and reparsed the same way as the @Nothing@ cases.
      , pure $ C.PrimSignImplicit Nothing
      ]

{-------------------------------------------------------------------------------
  Enumerate canonical C types
-------------------------------------------------------------------------------}

instance Enumerate CanonicalPrimType where
  enumerate = CanonicalPrimType <$> enumerate

instance Enumerate CanonicalComplexType where
  enumerate = CanonicalComplexType <$> enumerate

{-------------------------------------------------------------------------------
  Enumerate non-canonical C types
-------------------------------------------------------------------------------}

instance Enumerate NonCanonicalPrimType where
  enumerate = List.nub $ concatMap permute canonicals
    where
      permute (str, canonical) = [
            NonCanonicalPrimType (unwords strs') canonical
          | strs' <- List.permutations $ words str
          ]

      canonicals = [
            ( "char"
            , C.PrimChar (C.PrimSignImplicit Nothing)
            )
          , ( "signed char"
            , C.PrimChar (C.PrimSignExplicit C.Signed)
            )
          , ( "unsigned char"
            , C.PrimChar (C.PrimSignExplicit C.Unsigned)
            )
          , ( "short"
            , C.PrimIntegral C.PrimShort C.Signed
            )
          , ( "short int"
            , C.PrimIntegral C.PrimShort C.Signed
            )
          , ( "signed short"
            , C.PrimIntegral C.PrimShort C.Signed
            )
          , ( "signed short int"
            , C.PrimIntegral C.PrimShort C.Signed
            )
          , ( "unsigned short"
            , C.PrimIntegral C.PrimShort C.Unsigned
            )
          , ( "unsigned short int"
            , C.PrimIntegral C.PrimShort C.Unsigned
            )
          , ( "int"
            , C.PrimIntegral C.PrimInt C.Signed
            )
          , ( "signed"
            , C.PrimIntegral C.PrimInt C.Signed
            )
          , ( "signed int"
            , C.PrimIntegral C.PrimInt C.Signed
            )
          , ( "unsigned"
            , C.PrimIntegral C.PrimInt C.Unsigned
            )
          , ( "unsigned int"
            , C.PrimIntegral C.PrimInt C.Unsigned
            )
          , ( "long"
            , C.PrimIntegral C.PrimLong C.Signed
            )
          , ( "long int"
            , C.PrimIntegral C.PrimLong C.Signed
            )
          , ( "signed long"
            , C.PrimIntegral C.PrimLong C.Signed
            )
          , ( "signed long int"
            , C.PrimIntegral C.PrimLong C.Signed
            )
          , ( "unsigned long"
            , C.PrimIntegral C.PrimLong C.Unsigned
            )
          , ( "unsigned long int"
            , C.PrimIntegral C.PrimLong C.Unsigned
            )
          , ( "long long"
            , C.PrimIntegral C.PrimLongLong C.Signed
            )
          , ( "long long int"
            , C.PrimIntegral C.PrimLongLong C.Signed
            )
          , ( "signed long long"
            , C.PrimIntegral C.PrimLongLong C.Signed
            )
          , ( "signed long long int"
            , C.PrimIntegral C.PrimLongLong C.Signed
            )
          , ( "unsigned long long"
            , C.PrimIntegral C.PrimLongLong C.Unsigned
            )
          , ( "unsigned long long int"
            , C.PrimIntegral C.PrimLongLong C.Unsigned
            )
          , ( "float"
            , C.PrimFloating C.PrimFloat
            )
          , ( "double"
            , C.PrimFloating C.PrimDouble
            )
          -- NOTE: @long double@ is not supported by the reparser. See
          -- 'prop_reparseGlobal_longDouble' for a dedicated test instead.
          , ( "_Bool"
            , C.PrimBool
            )
          ]

instance Enumerate NonCanonicalComplexType where
  enumerate = List.nub $ concatMap permute canonicals
    where
      permute (str, canonical) = [
          NonCanonicalComplexType (unwords strs') canonical
        | strs' <- List.permutations $ words str
        ]

      canonicals = [
          ( "float _Complex"
          , C.TypeComplex (C.PrimFloating C.PrimFloat)
          )
        , ( "double _Complex"
          , C.TypeComplex (C.PrimFloating C.PrimDouble)
          )
          -- NOTE: @long double _Complex@ is not supported by the reparser. See
          -- 'prop_reparseGlobal_longDouble' for a dedicated test instead.
        ]

{-------------------------------------------------------------------------------
  Tokenization
-------------------------------------------------------------------------------}

-- | Tokenize a string representing the contents of a header file
--
-- This tokenizes *all* the contents of the header file. If you want to tokenize
-- only select declarations, you will have to define a variant of this function
-- that uses 'HighLevel.clang_visitChildren' to parse and tokenize individual
-- declarations.
tokenize :: String -> IO [Token TokenSpelling]
tokenize contents = withClang contents $ \unit -> do
    root <- clang_getTranslationUnitCursor unit
    range <- HighLevel.clang_getCursorExtent root
    HighLevel.clang_tokenize unit (multiLocExpansion <$> range)

{-------------------------------------------------------------------------------
  Call clang
-------------------------------------------------------------------------------}

data ClangError =
    ClangErrorDiagnostics [Diagnostic]
  | ClangErrorCode ErrorCode
  deriving stock (Show, Eq)
  deriving anyclass Exception

withClang :: forall a.
     String
  -> (CXTranslationUnit -> IO a)
  -> IO a
withClang contents k = do
    mRes <- withClang' contents $ \unit -> do
      diags <- HighLevel.clang_getDiagnostics unit Nothing
      if any diagnosticIsError diags
      then throwIO $ ClangErrorDiagnostics diags
      else k unit
    case mRes of
      Left e  -> throwIO $ ClangErrorCode e
      Right res -> pure res
  where

type ErrorCode = SimpleEnum CXErrorCode

withClang' :: forall a.
     String
  -> (CXTranslationUnit -> IO a)
  -> IO (Either ErrorCode a)
withClang' contents k =
    HighLevel.withIndex dispDiags $ \index ->
    HighLevel.withUnsavedFile path contents $ \file  ->
      withUnit index file
  where
    dispDiags :: DisplayDiagnostics
    dispDiags = DontDisplayDiagnostics

    args :: ClangArgs
    args = def

    flags :: BitfieldEnum CXTranslationUnit_Flags
    flags = bitfieldEnum [CXTranslationUnit_DetailedPreprocessingRecord]

    path :: FilePath
    path = "virtual.h"

    onErrorCode :: SimpleEnum CXErrorCode -> IO (Either ErrorCode a)
    onErrorCode err = pure $ Left err

    withUnit :: CXIndex -> CXUnsavedFile -> IO (Either ErrorCode a)
    withUnit index unsaved =
      HighLevel.withTranslationUnit2
        index
        (Just $ SourcePath $ Text.pack path)
        args
        [unsaved]
        flags
        onErrorCode
        (fmap Right . k)
