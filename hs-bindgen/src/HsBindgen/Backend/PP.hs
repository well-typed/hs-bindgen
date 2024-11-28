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

-- | @GHC.Float@ import
iGHCFloat :: HsImport
iGHCFloat = HsImport "GHC.Float" (Just "GHC.Float")

-- | @Data.Type.Equality@ import
iDataTypeEquality :: HsImport
iDataTypeEquality = HsImport "Data.Type.Equality" Nothing

-- | @HsBindgen.Syntaxc@ import
iHsBindgenSyntax :: HsImport
iHsBindgenSyntax = HsImport "HsBindgen.Syntax" (Just "HsBindgen")

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
    Foreign_FunPtr       -> importQ iForeign "FunPtr"
    ConstantArray        -> importQ (HsImport "HsBindgen.ConstantArray" Nothing) "ConstantArray"
    IO_type              -> import_ iPrelude "IO"

    NomEq_class     -> import_ iDataTypeEquality  "~"

    CNot_class             -> importQ iHsBindgenSyntax "CNot"
    CNot_resTyCon          -> importQ iHsBindgenSyntax "NotRes"
    CNot_not               -> importQ iHsBindgenSyntax "not"
    CLogical_class         -> importQ iHsBindgenSyntax "CLogical"
    CLogical_resTyCon      -> importQ iHsBindgenSyntax "LogicalRes"
    CLogical_and           -> import_ iHsBindgenSyntax "&&"
    CLogical_or            -> import_ iHsBindgenSyntax "||"
    CEq_class              -> importQ iHsBindgenSyntax "CEq"
    CEq_eq                 -> import_ iHsBindgenSyntax "=="
    CEq_uneq               -> import_ iHsBindgenSyntax "/="
    COrd_class             -> importQ iHsBindgenSyntax "COrd"
    COrd_lt                -> import_ iHsBindgenSyntax "<"
    COrd_le                -> import_ iHsBindgenSyntax "<="
    COrd_gt                -> import_ iHsBindgenSyntax ">"
    COrd_ge                -> import_ iHsBindgenSyntax ">="
    CPlus_class            -> importQ iHsBindgenSyntax "CPlus"
    CPlus_resTyCon         -> importQ iHsBindgenSyntax "PlusRes"
    CPlus_plus             -> importQ iHsBindgenSyntax "plus"
    CMinus_class           -> importQ iHsBindgenSyntax "CMinus"
    CMinus_resTyCon        -> importQ iHsBindgenSyntax "MinusRes"
    CMinus_negate          -> importQ iHsBindgenSyntax "negate"
    CAdd_class             -> importQ iHsBindgenSyntax "CAdd"
    CAdd_resTyCon          -> importQ iHsBindgenSyntax "AddRes"
    CAdd_add               -> import_ iHsBindgenSyntax "+"
    CSub_class             -> importQ iHsBindgenSyntax "CSub"
    CSub_resTyCon          -> importQ iHsBindgenSyntax "SubRes"
    CSub_minus             -> import_ iHsBindgenSyntax "-"
    CMult_class            -> importQ iHsBindgenSyntax "CMult"
    CMult_resTyCon         -> importQ iHsBindgenSyntax "MultRes"
    CMult_mult             -> import_ iHsBindgenSyntax "*"
    CDiv_class             -> importQ iHsBindgenSyntax "CDiv"
    CDiv_resTyCon          -> importQ iHsBindgenSyntax "DivRes"
    CDiv_div               -> import_ iHsBindgenSyntax "/"
    CRem_class             -> importQ iHsBindgenSyntax "CRem"
    CRem_resTyCon          -> importQ iHsBindgenSyntax "RemRes"
    CRem_rem               -> import_ iHsBindgenSyntax "rem"
    CComplement_class      -> importQ iHsBindgenSyntax "CComplement"
    CComplement_resTyCon   -> importQ iHsBindgenSyntax "ComplementRes"
    CComplement_complement -> importQ iHsBindgenSyntax "complement"
    CBits_class            -> importQ iHsBindgenSyntax "CBits"
    CBits_resTyCon         -> importQ iHsBindgenSyntax "BitsRes"
    CBits_and              -> import_ iHsBindgenSyntax ".&."
    CBits_or               -> import_ iHsBindgenSyntax ".|."
    CBits_xor              -> import_ iHsBindgenSyntax "xor"
    CShift_class           -> importQ iHsBindgenSyntax "CShift"
    CShift_resTyCon        -> importQ iHsBindgenSyntax "ShiftRes"
    CShift_shiftL          -> import_ iHsBindgenSyntax "shiftL"
    CShift_shiftR          -> import_ iHsBindgenSyntax "shiftR"

    IntLike_tycon    -> importQ iHsBindgenSyntax "IntLike"
    FloatLike_tycon  -> importQ iHsBindgenSyntax "FloatLike"

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
