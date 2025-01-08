module HsBindgen.Backend.PP (
    -- * Imports
    HsImportModule(..)
  , HsImport(..)
    -- * ResolvedName
  , ResolvedName(..)
  , NameType(..)
    -- * Resolution
  , resolveGlobal
    -- * BackendName
  , BackendName(..)
  ) where

import Data.Char qualified as Char

import HsBindgen.SHs.AST
import HsBindgen.Hs.AST.Type

{-------------------------------------------------------------------------------
  Imports
-------------------------------------------------------------------------------}

-- | An import module with an optional alias
data HsImportModule = HsImportModule {
      hsImportModuleName  :: String
    , hsImportModuleAlias :: Maybe String
    }
  deriving (Eq, Ord, Show)

-- | @Data.Bits@ import module
iBits :: HsImportModule
iBits = HsImportModule "Data.Bits" (Just "DB")

-- | @HsBindgen.ConstantArray@ import module
iConstantArray :: HsImportModule
iConstantArray = HsImportModule "HsBindgen.ConstantArray" Nothing

iFlexibleArrayMember :: HsImportModule
iFlexibleArrayMember = HsImportModule "HsBindgen.Patterns.FlexibleArrayMember" Nothing

-- | @Data.Void@ import module
iDataVoid :: HsImportModule
iDataVoid = HsImportModule "Data.Void" Nothing

-- | @Foreign@ import module
iForeign :: HsImportModule
iForeign = HsImportModule "Foreign" (Just "F")

-- | @Foreign.C@ import module
iForeignC :: HsImportModule
iForeignC = HsImportModule "Foreign.C" (Just "FC")

-- | @GHC.Float@ import module
iGHCFloat :: HsImportModule
iGHCFloat = HsImportModule "GHC.Float" (Just "GHC.Float")

-- | @HsBindgen.Patterns@ import module
iHsBindgenPatterns :: HsImportModule
iHsBindgenPatterns = HsImportModule "HsBindgen.Patterns" (Just "HsBindgen")

-- | @Prelude@ import module
iPrelude :: HsImportModule
iPrelude = HsImportModule "Prelude" (Just "P")

-- | A qualified or unqualified import of a module
data HsImport =
    QualifiedHsImport   HsImportModule
  | UnqualifiedHsImport HsImportModule
  deriving (Eq, Ord, Show)

{-------------------------------------------------------------------------------
  ResolvedName
-------------------------------------------------------------------------------}

-- | Resolved name
data ResolvedName = ResolvedName {
      resolvedNameString :: String
    , resolvedNameType   :: NameType
    , resolvedNameImport :: Maybe HsImport
    }
  deriving (Eq, Ord, Show)

-- | Construct a `ResvoledName` with no import
noImport :: String -> ResolvedName
noImport s = ResolvedName {
      resolvedNameString = s
    , resolvedNameType   = nameType s
    , resolvedNameImport = Nothing
    }

-- | Construct a `ResolvedName` with qualified import
importQ :: HsImportModule -> String -> ResolvedName
importQ hsImportModule s = ResolvedName {
      resolvedNameString = s
    , resolvedNameType   = nameType s
    , resolvedNameImport = Just $ QualifiedHsImport hsImportModule
    }

importU :: HsImportModule -> String -> ResolvedName
importU hsImportModule s = ResolvedName {
      resolvedNameString = s
    , resolvedNameType   = nameType s
    , resolvedNameImport = Just $ UnqualifiedHsImport hsImportModule
    }

-- | Name type
data NameType = IdentifierName | OperatorName
  deriving (Eq, Ord, Show)

nameType :: String -> NameType
nameType nm
  | all isIdentChar nm = IdentifierName
  | otherwise          = OperatorName
  where
    isIdentChar :: Char -> Bool
    isIdentChar c = Char.isAlphaNum c || c == '_' || c == '\''

{-------------------------------------------------------------------------------
  Resolution
-------------------------------------------------------------------------------}

-- | Resolve a 'Global'
resolveGlobal :: Global -> ResolvedName
resolveGlobal = \case
    -- When adding a new global that resolves to a non-qualified identifier, be
    -- sure to reserve the name in "HsBindgen.Hs.AST.Name".
    Unit_type            -> noImport "()"
    Unit_constructor     -> noImport "()"
    Applicative_pure     -> importU iPrelude "pure"
    Applicative_seq      -> importU iPrelude "<*>"
    Monad_return         -> importU iPrelude "return"
    Monad_seq            -> importU iPrelude ">>"
    Storable_Storable    -> importQ iForeign "Storable"
    Storable_sizeOf      -> importQ iForeign "sizeOf"
    Storable_alignment   -> importQ iForeign "alignment"
    Storable_peekByteOff -> importQ iForeign "peekByteOff"
    Storable_pokeByteOff -> importQ iForeign "pokeByteOff"
    Storable_peek        -> importQ iForeign "peek"
    Storable_poke        -> importQ iForeign "poke"
    Foreign_Ptr          -> importQ iForeign "Ptr"
    Foreign_FunPtr       -> importQ iForeign "FunPtr"
    ConstantArray        -> importQ iConstantArray "ConstantArray"
    IO_type              -> importU iPrelude "IO"
    HasFlexibleArrayMember_class -> importQ iFlexibleArrayMember "HasFlexibleArrayMember"
    HasFlexibleArrayMember_offset -> importQ iFlexibleArrayMember "flexibleArrayMemberOffset"

    Eq_class         -> importQ iPrelude           "Eq"
    Ord_class        -> importQ iPrelude           "Ord"
    Num_class        -> importQ iPrelude           "Num"
    Integral_class   -> importQ iPrelude           "Integral"
    Fractional_class -> importQ iPrelude           "Fractional"
    Div_class        -> importQ iHsBindgenPatterns "Div"
    Bits_class       -> importQ iBits              "Bits"

    Eq_eq           -> importQ iPrelude           "=="
    Eq_uneq         -> importQ iPrelude           "/="
    Ord_lt          -> importQ iPrelude           "<"
    Ord_le          -> importQ iPrelude           "<="
    Ord_gt          -> importQ iPrelude           ">"
    Ord_ge          -> importQ iPrelude           ">="
    Base_identity   -> importQ iPrelude           "id"
    Base_not        -> importQ iPrelude           "not"
    Base_and        -> importQ iPrelude           "&&"
    Base_or         -> importQ iPrelude           "||"
    Bits_shiftL     -> importQ iBits              "shiftL"
    Bits_shiftR     -> importQ iBits              "shiftR"
    Bits_and        -> importQ iBits              ".&."
    Bits_xor        -> importQ iBits              "xor"
    Bits_or         -> importQ iBits              ".|."
    Bits_complement -> importQ iBits              "complement"
    Num_negate      -> importQ iPrelude           "negate"
    Num_add         -> importQ iPrelude           "+"
    Num_minus       -> importQ iPrelude           "-"
    Num_times       -> importQ iPrelude           "*"
    Div_div         -> importQ iHsBindgenPatterns "/"
    Integral_rem    -> importQ iPrelude           "rem"
    GHC_Float_castWord32ToFloat -> importQ iGHCFloat "castWord32ToFloat"
    GHC_Float_castWord64ToDouble -> importQ iGHCFloat "castWord64ToDouble"
    CFloat_constructor -> importQ iForeignC "CFloat"
    CDouble_constructor -> importQ iForeignC "CDouble"

    PrimType hsPrimType  -> case hsPrimType of
      HsPrimVoid    -> importU iDataVoid "Void"
      HsPrimCChar   -> importQ iForeignC "CChar"
      HsPrimCSChar  -> importQ iForeignC "CSChar"
      HsPrimCUChar  -> importQ iForeignC "CUChar"
      HsPrimCInt    -> importQ iForeignC "CInt"
      HsPrimCUInt   -> importQ iForeignC "CUInt"
      HsPrimCShort  -> importQ iForeignC "CShort"
      HsPrimCUShort -> importQ iForeignC "CUShort"
      HsPrimCLong   -> importQ iForeignC "CLong"
      HsPrimCULong  -> importQ iForeignC "CULong"
      HsPrimCLLong  -> importQ iForeignC "CLLong"
      HsPrimCULLong -> importQ iForeignC "CULLong"
      HsPrimCBool   -> importQ iForeignC "CBool"
      HsPrimCFloat  -> importQ iForeignC "CFloat"
      HsPrimCDouble -> importQ iForeignC "CDouble"

{-------------------------------------------------------------------------------
  BackendName
-------------------------------------------------------------------------------}

-- | Backend name representation
data BackendName =
    -- | Local (not imported) name
    LocalBackendName NameType String
  | -- | Resolved name
    ResolvedBackendName ResolvedName
  deriving (Eq, Show)
