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

-- | @C.Typing@ import module
iCTyping :: HsImportModule
iCTyping = HsImportModule "C.Typing" (Just "C")

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
    Eq_class             -> importU iPrelude "Eq"
    Ord_class            -> importU iPrelude "Ord"
    Enum_class           -> importU iPrelude "Enum"
    Ix_class             -> importU iDataIx  "Ix"
    Bounded_class        -> importU iPrelude "Bounded"
    Read_class           -> importU iPrelude "Read"
    Show_class           -> importU iPrelude "Show"
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

    NomEq_class          -> importU iDataTypeEquality  "~"

    Not_class             -> importQ iCTyping "Not"
    Not_not               -> importQ iCTyping "not"
    Logical_class         -> importQ iCTyping "Logical"
    Logical_and           -> importU iCTyping "&&"
    Logical_or            -> importU iCTyping "||"
    RelEq_class           -> importQ iCTyping "RelEq"
    RelEq_eq              -> importU iCTyping "=="
    RelEq_uneq            -> importU iCTyping "!="
    RelOrd_class          -> importQ iCTyping "RelOrd"
    RelOrd_lt             -> importU iCTyping "<"
    RelOrd_le             -> importU iCTyping "<="
    RelOrd_gt             -> importU iCTyping ">"
    RelOrd_ge             -> importU iCTyping ">="
    Plus_class            -> importQ iCTyping "Plus"
    Plus_resTyCon         -> importQ iCTyping "PlusRes"
    Plus_plus             -> importQ iCTyping "plus"
    Minus_class           -> importQ iCTyping "Minus"
    Minus_resTyCon        -> importQ iCTyping "MinusRes"
    Minus_negate          -> importQ iCTyping "negate"
    Add_class             -> importQ iCTyping "Add"
    Add_resTyCon          -> importQ iCTyping "AddRes"
    Add_add               -> importU iCTyping "+"
    Sub_class             -> importQ iCTyping "Sub"
    Sub_resTyCon          -> importQ iCTyping "SubRes"
    Sub_minus             -> importU iCTyping "-"
    Mult_class            -> importQ iCTyping "Mult"
    Mult_resTyCon         -> importQ iCTyping "MultRes"
    Mult_mult             -> importU iCTyping "*"
    Div_class             -> importQ iCTyping "Div"
    Div_resTyCon          -> importQ iCTyping "DivRes"
    Div_div               -> importU iCTyping "/"
    Rem_class             -> importQ iCTyping "Rem"
    Rem_resTyCon          -> importQ iCTyping "RemRes"
    Rem_rem               -> importU iCTyping "%"
    Complement_class      -> importQ iCTyping "Complement"
    Complement_resTyCon   -> importQ iCTyping "ComplementRes"
    Complement_complement -> importQ iCTyping ".~"
    Bitwise_class         -> importQ iCTyping "Bitwise"
    Bitwise_resTyCon      -> importQ iCTyping "BitsRes"
    Bitwise_and           -> importU iCTyping ".&."
    Bitwise_or            -> importU iCTyping ".|."
    Bitwise_xor           -> importU iCTyping ".^."
    Shift_class           -> importQ iCTyping "Shift"
    Shift_resTyCon        -> importQ iCTyping "ShiftRes"
    Shift_shiftL          -> importU iCTyping "<<"
    Shift_shiftR          -> importU iCTyping ">>"

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
