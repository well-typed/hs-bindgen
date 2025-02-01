module HsBindgen.Backend.PP.Names (
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

import HsBindgen.Clang.Args (Target(..))
import HsBindgen.Hs.AST.Type
import HsBindgen.SHs.AST

{-------------------------------------------------------------------------------
  Imports
-------------------------------------------------------------------------------}

-- | An import module with an optional alias
data HsImportModule = HsImportModule {
      hsImportModuleName  :: String
    , hsImportModuleAlias :: Maybe String
    }
  deriving (Eq, Ord, Show)

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

-- | @Data.Type.Equality@ import module
iDataTypeEquality :: HsImportModule
iDataTypeEquality = HsImportModule "Data.Type.Equality" Nothing

-- | @HsBindgen.Syntax@ import module
iHsBindgenSyntax :: HsImportModule
iHsBindgenSyntax = HsImportModule "HsBindgen.Syntax" (Just "HsBindgen")

-- | @Prelude@ import module
iPrelude :: HsImportModule
iPrelude = HsImportModule "Prelude" (Just "P")

-- | @Data.Ix@ import module
iDataIx :: HsImportModule
iDataIx = HsImportModule "Data.Ix" (Just "Ix")

-- | @Data.Bits@ import module
iDataBits :: HsImportModule
iDataBits = HsImportModule "Data.Bits" (Just "Bits")

-- | @C.Expr.*@ platform specific import module
iCExprFor :: Target -> HsImportModule
iCExprFor = \case
    Target_Linux_X86_64   -> HsImportModule "C.Expr.Posix64" (Just "CExpr")
    Target_Linux_AArch64  -> HsImportModule "C.Expr.Posix64" (Just "CExpr")
    Target_Linux_X86      -> HsImportModule "C.Expr.Posix32" (Just "CExpr")
    Target_MacOS_X86_64   -> HsImportModule "C.Expr.Posix64" (Just "CExpr")
    Target_MacOS_AArch64  -> HsImportModule "C.Expr.Posix64" (Just "CExpr")
    Target_Windows_X86_64 -> HsImportModule "C.Expr.Win64"   (Just "CExpr")

{-------------------------------------------------------------------------------
  NameType
-------------------------------------------------------------------------------}

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

-- | Construct a `ResolvedName` with no import
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
  | nm == "()"         = IdentifierName
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
    Storable_class       -> importQ iForeign "Storable"
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

    Bits_class       -> importQ iDataBits "Bits"
    Bounded_class    -> importU iPrelude  "Bounded"
    Enum_class       -> importU iPrelude  "Enum"
    Eq_class         -> importU iPrelude  "Eq"
    FiniteBits_class -> importU iDataBits "FiniteBits"
    Floating_class   -> importU iPrelude  "Floating"
    Fractional_class -> importU iPrelude  "Fractional"
    Integral_class   -> importU iPrelude  "Integral"
    Ix_class         -> importQ iDataIx   "Ix"
    Num_class        -> importU iPrelude  "Num"
    Ord_class        -> importU iPrelude  "Ord"
    Read_class       -> importU iPrelude  "Read"
    Real_class       -> importU iPrelude  "Real"
    RealFloat_class  -> importU iPrelude  "RealFloat"
    RealFrac_class   -> importU iPrelude  "RealFrac"
    Show_class       -> importU iPrelude  "Show"

    NomEq_class -> importU iDataTypeEquality  "~"

    CExpr target name -> resolveGlobalCExpr target name

    IntLike_tycon    -> importQ iHsBindgenSyntax "IntLike"
    FloatLike_tycon  -> importQ iHsBindgenSyntax "FloatLike"

    GHC_Float_castWord32ToFloat -> importQ iGHCFloat "castWord32ToFloat"
    GHC_Float_castWord64ToDouble -> importQ iGHCFloat "castWord64ToDouble"
    CFloat_constructor -> importQ iForeignC "CFloat"
    CDouble_constructor -> importQ iForeignC "CDouble"

    PrimType hsPrimType  -> case hsPrimType of
      HsPrimVoid     -> importU iDataVoid "Void"
      HsPrimUnit     -> noImport "()"
      HsPrimCChar    -> importQ iForeignC "CChar"
      HsPrimCSChar   -> importQ iForeignC "CSChar"
      HsPrimCUChar   -> importQ iForeignC "CUChar"
      HsPrimCInt     -> importQ iForeignC "CInt"
      HsPrimCUInt    -> importQ iForeignC "CUInt"
      HsPrimCShort   -> importQ iForeignC "CShort"
      HsPrimCUShort  -> importQ iForeignC "CUShort"
      HsPrimCLong    -> importQ iForeignC "CLong"
      HsPrimCULong   -> importQ iForeignC "CULong"
      HsPrimCLLong   -> importQ iForeignC "CLLong"
      HsPrimCULLong  -> importQ iForeignC "CULLong"
      HsPrimCBool    -> importQ iForeignC "CBool"
      HsPrimCFloat   -> importQ iForeignC "CFloat"
      HsPrimCDouble  -> importQ iForeignC "CDouble"
      HsPrimCPtrDiff -> importQ iForeignC "CPtrdiff"

resolveGlobalCExpr :: Target -> GlobalCExpr -> ResolvedName
resolveGlobalCExpr target = \case
    Not_class             -> importQ iCExpr "Not"
    Not_not               -> importQ iCExpr "not"
    Logical_class         -> importQ iCExpr "Logical"
    Logical_and           -> importU iCExpr "&&"
    Logical_or            -> importU iCExpr "||"
    RelEq_class           -> importQ iCExpr "RelEq"
    RelEq_eq              -> importU iCExpr "=="
    RelEq_uneq            -> importU iCExpr "!="
    RelOrd_class          -> importQ iCExpr "RelOrd"
    RelOrd_lt             -> importU iCExpr "<"
    RelOrd_le             -> importU iCExpr "<="
    RelOrd_gt             -> importU iCExpr ">"
    RelOrd_ge             -> importU iCExpr ">="
    Plus_class            -> importQ iCExpr "Plus"
    Plus_resTyCon         -> importQ iCExpr "PlusRes"
    Plus_plus             -> importQ iCExpr "plus"
    Minus_class           -> importQ iCExpr "Minus"
    Minus_resTyCon        -> importQ iCExpr "MinusRes"
    Minus_negate          -> importQ iCExpr "negate"
    Add_class             -> importQ iCExpr "Add"
    Add_resTyCon          -> importQ iCExpr "AddRes"
    Add_add               -> importU iCExpr "+"
    Sub_class             -> importQ iCExpr "Sub"
    Sub_resTyCon          -> importQ iCExpr "SubRes"
    Sub_minus             -> importU iCExpr "-"
    Mult_class            -> importQ iCExpr "Mult"
    Mult_resTyCon         -> importQ iCExpr "MultRes"
    Mult_mult             -> importU iCExpr "*"
    Div_class             -> importQ iCExpr "Div"
    Div_resTyCon          -> importQ iCExpr "DivRes"
    Div_div               -> importU iCExpr "/"
    Rem_class             -> importQ iCExpr "Rem"
    Rem_resTyCon          -> importQ iCExpr "RemRes"
    Rem_rem               -> importU iCExpr "%"
    Complement_class      -> importQ iCExpr "Complement"
    Complement_resTyCon   -> importQ iCExpr "ComplementRes"
    Complement_complement -> importQ iCExpr ".~"
    Bitwise_class         -> importQ iCExpr "Bitwise"
    Bitwise_resTyCon      -> importQ iCExpr "BitsRes"
    Bitwise_and           -> importU iCExpr ".&."
    Bitwise_or            -> importU iCExpr ".|."
    Bitwise_xor           -> importU iCExpr ".^."
    Shift_class           -> importQ iCExpr "Shift"
    Shift_resTyCon        -> importQ iCExpr "ShiftRes"
    Shift_shiftL          -> importU iCExpr "<<"
    Shift_shiftR          -> importU iCExpr ">>"
  where
    iCExpr :: HsImportModule
    iCExpr = iCExprFor target

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
