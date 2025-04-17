{-# LANGUAGE TemplateHaskellQuotes #-}
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
import Data.List qualified as L
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map

import HsBindgen.Imports
import HsBindgen.SHs.AST
import HsBindgen.Hs.AST.Type

import Language.Haskell.TH.Syntax qualified as TH

import C.Char ( CharValue(..), charValueFromAddr )
import C.Expr.HostPlatform qualified
import Data.Bits qualified
import Data.Ix qualified
import Data.Void qualified
import Foreign qualified
import Foreign.C qualified
import Foreign.C.String qualified
import GHC.Float qualified
import GHC.Ptr qualified
import HsBindgen.Runtime.Bitfield qualified
import HsBindgen.Runtime.ByteArray qualified
import HsBindgen.Runtime.ConstantArray qualified
import HsBindgen.Runtime.CEnum qualified
import HsBindgen.Runtime.FlexibleArrayMember qualified
import HsBindgen.Runtime.Syntax qualified
import HsBindgen.Runtime.SizedByteArray qualified

{-------------------------------------------------------------------------------
  Imports
-------------------------------------------------------------------------------}

-- | An import module with an optional alias
data HsImportModule = HsImportModule {
      hsImportModuleName  :: String
    , hsImportModuleAlias :: Maybe String
    }
  deriving (Eq, Ord, Show)

-- | @Prelude@ import module
iPrelude :: HsImportModule
iPrelude = HsImportModule "Prelude" (Just "P")

{-------------------------------------------------------------------------------
  NameType
-------------------------------------------------------------------------------}

-- | A qualified or unqualified import of a module
data HsImport =
    QualifiedHsImport   HsImportModule
  | UnqualifiedHsImport HsImportModule
  deriving (Eq, Ord, Show)

-- | Resolved name
data ResolvedName = ResolvedName {
      resolvedNameString :: String
    , resolvedNameType   :: NameType
    , resolvedNameImport :: Maybe HsImport
    }
  deriving (Eq, Ord, Show)

-- | Name for tuples
tupleResolvedName :: Bool -> Word -> ResolvedName
tupleResolvedName wantType i = ResolvedName tup IdentifierName Nothing
  where
    tup
      | i == 0
      = "()"
      | i == 1
      = if wantType
        then "Solo"
        else "MkSolo"
      | otherwise
      = "(" ++ replicate (fromIntegral (i - 1)) ',' ++ ")"

{-------------------------------------------------------------------------------
  Imports helpers
-------------------------------------------------------------------------------}

importQ :: TH.Name -> ResolvedName
importQ name = ResolvedName
    { resolvedNameString = s
    , resolvedNameType   = nameType s
    , resolvedNameImport = fmap (QualifiedHsImport . moduleOf s) (TH.nameModule name)
    }
  where
    s = TH.nameBase name

importU :: TH.Name -> ResolvedName
importU name = ResolvedName
    { resolvedNameString = s
    , resolvedNameType   = nameType s
    , resolvedNameImport = fmap (UnqualifiedHsImport . moduleOf s) (TH.nameModule name)
    }
  where
    s = TH.nameBase name

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

-- | Create 'HsImportModule' from a definition module.
--
-- We need to map internal modules to external ones for @base@ names.
moduleOf :: String -> String -> HsImportModule
moduleOf "Void"       _ = HsImportModule "Data.Void" Nothing
moduleOf "CStringLen" _ =
  -- We want the same qualifier whether we get CStringLen
  -- from Foreign.C.String or GHC.Foreign, so special-case it here.
  HsImportModule "Foreign.C.String" (Just "FC")
moduleOf "NonEmpty" _ = HsImportModule "Data.List.NonEmpty" Nothing
moduleOf ":|"       _ = HsImportModule "Data.List.NonEmpty" Nothing
moduleOf _ident m0 = case parts of
    ["C","Operator","Classes"]       -> HsImportModule "C.Expr.HostPlatform" (Just "C")
    ["HsBindgen","Runtime","Syntax"] -> HsImportModule "HsBindgen.Runtime.Syntax" (Just "HsBindgen")
    ["GHC", "Bits"]                  -> HsImportModule "Data.Bits" (Just "Bits")
    ["GHC", "Base"]                  -> iPrelude
    ["GHC", "Classes"]               -> iPrelude
    ["GHC", "Show"]                  -> iPrelude
    ["GHC", "Types"]                 -> iPrelude
    ["GHC", "Read"]                  -> iPrelude
    ["GHC", "Real"]                  -> iPrelude
    ["GHC", "Enum"]                  -> iPrelude
    ["GHC", "Float"]                 -> iPrelude
    ["GHC", "Num"]                   -> iPrelude
    ["GHC", "Maybe"]                 -> iPrelude
    ["GHC", "Ix"]                    -> HsImportModule "Data.Ix" (Just "Ix")
    ("GHC" : "Foreign" : "C" : _)    -> HsImportModule "Foreign.C" (Just "FC")
    ("Foreign" : "C" : _)            -> HsImportModule "Foreign.C" (Just "FC")
    ["GHC", "Ptr"]                   -> HsImportModule "Foreign"   (Just "F")
    ("GHC" : "Foreign" : _)          -> HsImportModule "Foreign"   (Just "F")
    ("Foreign" : _)                  -> HsImportModule "Foreign"   (Just "F")

    -- otherwise just use module as is.
    _ -> HsImportModule (L.intercalate "." parts) Nothing
  where
    -- we drop "Internal" (to reduce ghc-internal migration noise)
    parts = filter ("Internal" /=) (split '.' m0)

split :: Eq a => a -> [a] -> [[a]]
split _ []      = []
split e (x:xs)
    | x == e    = [] : split e xs
    | otherwise = (x:pfx) : split e sfx
  where
    (pfx, sfx) = span' e xs

span' :: Eq a => a -> [a] -> ([a],[a])
span' _ []      = ([], [])
span' e (x:xs')
    | e == x    = ([], xs')
    | otherwise = let (ys,zs) = span' e xs' in (x:ys,zs)

{-------------------------------------------------------------------------------
  Resolution
-------------------------------------------------------------------------------}

-- | Resolve a 'Global'
resolveGlobal :: Global -> ResolvedName
resolveGlobal = \case
    -- When adding a new global that resolves to a non-qualified identifier, be
    -- sure to reserve the name in "HsBindgen.Hs.AST.Name".
    Tuple_type i         -> tupleResolvedName True  i
    Tuple_constructor i  -> tupleResolvedName False i
    Applicative_pure     -> importU 'pure
    Applicative_seq      -> importU '(<*>)
    Monad_return         -> importU 'return
    Monad_seq            -> importU '(>>)
    Storable_class       -> importQ ''Foreign.Storable
    Storable_sizeOf      -> importQ 'Foreign.sizeOf
    Storable_alignment   -> importQ 'Foreign.alignment
    Storable_peekByteOff -> importQ 'Foreign.peekByteOff
    Storable_pokeByteOff -> importQ 'Foreign.pokeByteOff
    Storable_peek        -> importQ 'Foreign.peek
    Storable_poke        -> importQ 'Foreign.poke
    Foreign_Ptr          -> importQ ''Foreign.Ptr
    Foreign_FunPtr       -> importQ ''Foreign.FunPtr
    Ptr_constructor      -> importQ ''GHC.Ptr.Ptr
    ConstantArray        -> importQ ''HsBindgen.Runtime.ConstantArray.ConstantArray
    IO_type              -> importU ''IO
    HasFlexibleArrayMember_class -> importQ ''HsBindgen.Runtime.FlexibleArrayMember.HasFlexibleArrayMember
    HasFlexibleArrayMember_offset -> importQ 'HsBindgen.Runtime.FlexibleArrayMember.flexibleArrayMemberOffset
    Bitfield_peekBitOffWidth -> importQ 'HsBindgen.Runtime.Bitfield.peekBitOffWidth
    Bitfield_pokeBitOffWidth -> importQ 'HsBindgen.Runtime.Bitfield.pokeBitOffWidth
    CharValue_tycon       -> importQ ''C.Char.CharValue
    CharValue_constructor -> importQ 'C.Char.CharValue
    CharValue_fromAddr    -> importQ 'C.Char.charValueFromAddr

    Bits_class       -> importQ ''Data.Bits.Bits
    Bounded_class    -> importU ''Bounded
    Enum_class       -> importU ''Enum
    Eq_class         -> importU ''Eq
    FiniteBits_class -> importU ''Data.Bits.FiniteBits
    Floating_class   -> importU ''Floating
    Fractional_class -> importU ''Fractional
    Integral_class   -> importU ''Integral
    Ix_class         -> importQ ''Data.Ix.Ix
    Num_class        -> importU ''Num
    Ord_class        -> importU ''Ord
    Read_class       -> importU ''Read
    Real_class       -> importU ''Real
    RealFloat_class  -> importU ''RealFloat
    RealFrac_class   -> importU ''RealFrac
    Show_class       -> importU ''Show
    Show_show        -> importU 'show

    -- We now import ~ from Prelude;
    -- but it's not always there; it's also not in Data.Type.Equality
    -- on GHC-9.2, it just exists.
    --
    -- https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0371-non-magical-eq.md
    --
    -- So for now code using ~ will not work with preprocessor setup on GHC-9.2.
    NomEq_class -> ResolvedName "~" OperatorName (Just (UnqualifiedHsImport iPrelude))

    Not_class             -> importQ ''C.Expr.HostPlatform.Not
    Not_not               -> importQ 'C.Expr.HostPlatform.not
    Logical_class         -> importQ ''C.Expr.HostPlatform.Logical
    Logical_and           -> importU '(C.Expr.HostPlatform.&&)
    Logical_or            -> importU '(C.Expr.HostPlatform.||)
    RelEq_class           -> importQ ''C.Expr.HostPlatform.RelEq
    RelEq_eq              -> importU '(C.Expr.HostPlatform.==)
    RelEq_uneq            -> importU '(C.Expr.HostPlatform.!=)
    RelOrd_class          -> importQ ''C.Expr.HostPlatform.RelOrd
    RelOrd_lt             -> importU '(C.Expr.HostPlatform.<)
    RelOrd_le             -> importU '(C.Expr.HostPlatform.<=)
    RelOrd_gt             -> importU '(C.Expr.HostPlatform.>)
    RelOrd_ge             -> importU '(C.Expr.HostPlatform.>=)
    Plus_class            -> importQ ''C.Expr.HostPlatform.Plus
    Plus_resTyCon         -> importQ ''C.Expr.HostPlatform.PlusRes
    Plus_plus             -> importQ 'C.Expr.HostPlatform.plus
    Minus_class           -> importQ ''C.Expr.HostPlatform.Minus
    Minus_resTyCon        -> importQ ''C.Expr.HostPlatform.MinusRes
    Minus_negate          -> importQ 'C.Expr.HostPlatform.negate
    Add_class             -> importQ ''C.Expr.HostPlatform.Add
    Add_resTyCon          -> importQ ''C.Expr.HostPlatform.AddRes
    Add_add               -> importU '(C.Expr.HostPlatform.+)
    Sub_class             -> importQ ''C.Expr.HostPlatform.Sub
    Sub_resTyCon          -> importQ ''C.Expr.HostPlatform.SubRes
    Sub_minus             -> importU '(C.Expr.HostPlatform.-)
    Mult_class            -> importQ ''C.Expr.HostPlatform.Mult
    Mult_resTyCon         -> importQ ''C.Expr.HostPlatform.MultRes
    Mult_mult             -> importU '(C.Expr.HostPlatform.*)
    Div_class             -> importQ ''C.Expr.HostPlatform.Div
    Div_resTyCon          -> importQ ''C.Expr.HostPlatform.DivRes
    Div_div               -> importU '(C.Expr.HostPlatform./)
    Rem_class             -> importQ ''C.Expr.HostPlatform.Rem
    Rem_resTyCon          -> importQ ''C.Expr.HostPlatform.RemRes
    Rem_rem               -> importU '(C.Expr.HostPlatform.%)
    Complement_class      -> importQ ''C.Expr.HostPlatform.Complement
    Complement_resTyCon   -> importQ ''C.Expr.HostPlatform.ComplementRes
    Complement_complement -> importQ '(C.Expr.HostPlatform..~)
    Bitwise_class         -> importQ ''C.Expr.HostPlatform.Bitwise
    Bitwise_resTyCon      -> importQ ''C.Expr.HostPlatform.BitsRes
    Bitwise_and           -> importU '(C.Expr.HostPlatform..&.)
    Bitwise_or            -> importU '(C.Expr.HostPlatform..|.)
    Bitwise_xor           -> importU '(C.Expr.HostPlatform..^.)
    Shift_class           -> importQ ''C.Expr.HostPlatform.Shift
    Shift_resTyCon        -> importQ ''C.Expr.HostPlatform.ShiftRes
    Shift_shiftL          -> importU '(C.Expr.HostPlatform.<<)
    Shift_shiftR          -> importU '(C.Expr.HostPlatform.>>)

    IntLike_tycon    -> importQ ''HsBindgen.Runtime.Syntax.IntLike
    FloatLike_tycon  -> importQ ''HsBindgen.Runtime.Syntax.FloatLike

    GHC_Float_castWord32ToFloat -> importQ 'GHC.Float.castWord32ToFloat
    GHC_Float_castWord64ToDouble -> importQ 'GHC.Float.castWord64ToDouble
    CFloat_constructor -> importQ ''Foreign.C.CFloat
    CDouble_constructor -> importQ ''Foreign.C.CDouble

    NonEmpty_constructor -> importQ '(NonEmpty.:|)
    NonEmpty_singleton   -> importQ 'NonEmpty.singleton
    Map_fromList         -> importQ 'Map.fromList

    CEnum_class -> importQ ''HsBindgen.Runtime.CEnum.CEnum
    CEnumZ_tycon -> importQ ''HsBindgen.Runtime.CEnum.CEnumZ
    CEnum_fromCEnumZ -> importQ 'HsBindgen.Runtime.CEnum.fromCEnumZ
    CEnum_toCEnumZ -> importQ 'HsBindgen.Runtime.CEnum.toCEnumZ
    CEnum_declaredValues -> importQ 'HsBindgen.Runtime.CEnum.declaredValues
    CEnum_isDeclared -> importQ 'HsBindgen.Runtime.CEnum.isDeclared
    CEnum_mkDeclared -> importQ 'HsBindgen.Runtime.CEnum.mkDeclared
    SequentialCEnum_class -> importQ ''HsBindgen.Runtime.CEnum.SequentialCEnum
    SequentialCEnum_minDeclaredValue -> importQ 'HsBindgen.Runtime.CEnum.minDeclaredValue
    SequentialCEnum_maxDeclaredValue -> importQ 'HsBindgen.Runtime.CEnum.maxDeclaredValue
    CEnum_showCEnum -> importQ 'HsBindgen.Runtime.CEnum.showCEnum
    CEnum_seqIsDeclared -> importQ 'HsBindgen.Runtime.CEnum.seqIsDeclared
    CEnum_seqMkDeclared -> importQ 'HsBindgen.Runtime.CEnum.seqMkDeclared
    AsCEnum_type -> importQ ''HsBindgen.Runtime.CEnum.AsCEnum
    AsSequentialCEnum_type -> importQ ''HsBindgen.Runtime.CEnum.AsSequentialCEnum

    ByteArray_type -> importQ ''ByteArray
    SizedByteArray_type -> importQ ''HsBindgen.Runtime.SizedByteArray.SizedByteArray

    ByteArray_getUnionPayload -> importQ 'HsBindgen.Runtime.ByteArray.getUnionPayload
    ByteArray_setUnionPayload -> importQ 'HsBindgen.Runtime.ByteArray.setUnionPayload

    PrimType hsPrimType  -> case hsPrimType of
      HsPrimVoid       -> importU ''Data.Void.Void
      HsPrimUnit       -> tupleResolvedName True 0
      HsPrimCChar      -> importQ ''Foreign.C.CChar
      HsPrimCSChar     -> importQ ''Foreign.C.CSChar
      HsPrimCUChar     -> importQ ''Foreign.C.CUChar
      HsPrimCInt       -> importQ ''Foreign.C.CInt
      HsPrimCUInt      -> importQ ''Foreign.C.CUInt
      HsPrimCShort     -> importQ ''Foreign.C.CShort
      HsPrimCUShort    -> importQ ''Foreign.C.CUShort
      HsPrimCLong      -> importQ ''Foreign.C.CLong
      HsPrimCULong     -> importQ ''Foreign.C.CULong
      HsPrimCLLong     -> importQ ''Foreign.C.CLLong
      HsPrimCULLong    -> importQ ''Foreign.C.CULLong
      HsPrimCBool      -> importQ ''Foreign.C.CBool
      HsPrimCFloat     -> importQ ''Foreign.C.CFloat
      HsPrimCDouble    -> importQ ''Foreign.C.CDouble
      HsPrimCPtrDiff   -> importQ ''Foreign.C.CPtrdiff
      HsPrimCStringLen -> importQ ''Foreign.C.String.CStringLen
      HsPrimInt        -> importU ''Int

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
