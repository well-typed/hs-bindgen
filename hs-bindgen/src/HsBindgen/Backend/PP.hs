{-# LANGUAGE RecordWildCards #-}

module HsBindgen.Backend.PP (
    -- * HsImport
    HsImport(..)
    -- * NameType
  , NameType(..)
    -- * ResolvedName
  , ResolvedName(..)
    -- * Resolution
  , resolveGlobal
    -- * BackendName
  , BackendName(..)
  ) where

import Data.Char qualified as Char

import HsBindgen.SHs.AST
import HsBindgen.Hs.AST.Type

{-------------------------------------------------------------------------------
  HsImport
-------------------------------------------------------------------------------}

-- | An import with an optional alias
data HsImport = HsImport {
      hsImportModule :: String
    , hsImportAlias  :: Maybe String
    }
  deriving (Eq, Ord, Show)

-- | @Data.Void@ import
iDataVoid :: HsImport
iDataVoid = HsImport "Data.Void" Nothing

-- | @Foreign@ import
iForeign :: HsImport
iForeign = HsImport "Foreign" (Just "F")

-- | @Foreign.C@ import
iForeignC :: HsImport
iForeignC = HsImport "Foreign.C" (Just "FC")

-- | @Prelude@ import
iPrelude :: HsImport
iPrelude = HsImport "Prelude" (Just "P")

-- | @Data.Bits@ import
iBits :: HsImport
iBits = HsImport "Data.Bits" (Just "DB")

-- | @GHC.Float@ import
iGHCFloat :: HsImport
iGHCFloat = HsImport "GHC.Float" (Just "GHC.Float")

-- | @Data.Type.Equality@ import
iDataTypeEquality :: HsImport
iDataTypeEquality = HsImport "Data.Type.Equality" Nothing

-- | @HsBindgen.Patterns@ import
iHsBindgenPatterns :: HsImport
iHsBindgenPatterns = HsImport "HsBindgen.Patterns" (Just "HsBindgen")

{-------------------------------------------------------------------------------
  NameType
-------------------------------------------------------------------------------}

-- | Name type
data NameType = IdentifierName | OperatorName
  deriving (Eq, Ord, Show)

{-------------------------------------------------------------------------------
  ResolvedName
-------------------------------------------------------------------------------}

-- | Resolved name
data ResolvedName = ResolvedName {
      resolvedNameString  :: String
    , resolvedNameImport  :: HsImport
    , resolvedNameQualify :: Bool
    , resolvedNameType    :: NameType
    }
  deriving (Eq, Ord, Show)

-- | Construct a 'ResolvedName'
mkResolvedName :: Bool -> HsImport -> String -> ResolvedName
mkResolvedName resolvedNameQualify resolvedNameImport resolvedNameString =
  let resolvedNameType = nameType resolvedNameString
  in  ResolvedName{..}

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
    Unit_type            -> import_ iPrelude "()"
    Unit_constructor     -> import_ iPrelude "()"
    Applicative_pure     -> import_ iPrelude "pure"
    Applicative_seq      -> import_ iPrelude "<*>"
    Monad_return         -> import_ iPrelude "return"
    Monad_seq            -> import_ iPrelude ">>"
    Storable_Storable    -> importQ iForeign "Storable"
    Storable_sizeOf      -> importQ iForeign "sizeOf"
    Storable_alignment   -> importQ iForeign "alignment"
    Storable_peekByteOff -> importQ iForeign "peekByteOff"
    Storable_pokeByteOff -> importQ iForeign "pokeByteOff"
    Storable_peek        -> importQ iForeign "peek"
    Storable_poke        -> importQ iForeign "poke"
    Foreign_Ptr          -> importQ iForeign "Ptr"
    ConstantArray        -> importQ (HsImport "HsBindgen.ConstantArray" Nothing) "ConstantArray"

    NomEq_class      -> import_ iDataTypeEquality  "~"
    Eq_class         -> importQ iPrelude           "Eq"
    Ord_class        -> importQ iPrelude           "Ord"
    Num_class        -> importQ iPrelude           "Num"
    Integral_class   -> importQ iPrelude           "Integral"
    Div_class        -> importQ iHsBindgenPatterns "Div"
    Bits_class       -> importQ iBits              "Bits"

    IntLike_tycon    -> importQ iHsBindgenPatterns "IntLike"
    FloatLike_tycon  -> importQ iHsBindgenPatterns "FloatLike"

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
    Unary_plus      -> importQ iHsBindgenPatterns "unaryPlus"
    GHC_Float_castWord32ToFloat -> importQ iGHCFloat "castWord32ToFloat"
    GHC_Float_castWord64ToDouble -> importQ iGHCFloat "castWord64ToDouble"
    CFloat_constructor -> importQ iForeignC "CFloat"
    CDouble_constructor -> importQ iForeignC "CDouble"

    PrimType hsPrimType  -> case hsPrimType of
      HsPrimVoid    -> import_ iDataVoid "Void"
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
  where
    import_, importQ :: HsImport -> String -> ResolvedName
    import_ = mkResolvedName False
    importQ = mkResolvedName True

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
