{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module HsBindgen.Backend.HsModule.Names (
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

import Data.Bits qualified
import Data.Char qualified as Char
import Data.Complex qualified as Complex
import Data.Int qualified
import Data.Ix qualified
import Data.List qualified as L
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Primitive.Types qualified as Primitive
import Data.Proxy qualified
import Data.Set qualified as Set
import Data.Void qualified
import Data.Word qualified
import Foreign qualified
import Foreign.C qualified
import Foreign.C.String qualified
import GHC.Base qualified
import GHC.Float qualified
import GHC.Records qualified
import Language.Haskell.TH.Syntax qualified as TH
import System.IO.Unsafe qualified
import Text.Read qualified

import C.Expr.HostPlatform qualified as CExpr.Runtime

import HsBindgen.Runtime.BitfieldPtr qualified
import HsBindgen.Runtime.Block qualified
import HsBindgen.Runtime.CEnum qualified
import HsBindgen.Runtime.FLAM qualified
import HsBindgen.Runtime.HasCBitfield qualified
import HsBindgen.Runtime.HasCField qualified
import HsBindgen.Runtime.Internal.Bitfield qualified
import HsBindgen.Runtime.Internal.ByteArray qualified
import HsBindgen.Runtime.Internal.HasFFIType qualified
import HsBindgen.Runtime.Internal.Prelude qualified as RP
import HsBindgen.Runtime.Internal.SizedByteArray qualified
import HsBindgen.Runtime.Internal.TypeEquality qualified
import HsBindgen.Runtime.Marshal qualified
import HsBindgen.Runtime.PtrConst qualified

import HsBindgen.Backend.SHs.AST
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Imports
-------------------------------------------------------------------------------}

-- TODO D: I think we don't need the alias anymore, so we can remove this type.
-- | An import module with an optional alias
data HsImportModule = HsImportModule {
      name  :: Hs.ModuleName
    , alias :: Maybe String
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

-- TODO D: We could add the original TH.Name here, then we don't need any
-- special functionality for TH mode.
--
-- | Resolved name
data ResolvedName = ResolvedName {
      string   :: String
    , typ      :: NameType
    , hsImport :: Maybe HsImport
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

-- TODO D: DEPRECATED
importQ :: TH.Name -> ResolvedName
importQ name = ResolvedName{
      string   = s
    , typ      = nameType s
    , hsImport = fmap (QualifiedHsImport . moduleOf s) (TH.nameModule name)
    }
  where
    s = TH.nameBase name

-- TODO D: DEPRECATED
importU :: TH.Name -> ResolvedName
importU name = ResolvedName{
      string   = s
    , typ      = nameType s
    , hsImport = fmap (UnqualifiedHsImport . moduleOf s) (TH.nameModule name)
    }
  where
    s = TH.nameBase name

-- | Unqualified import from "Prelude".
pU :: PGlobal -> ResolvedName
pU x = ResolvedName{
      string   = s
    , typ      = nameType s
    , hsImport =
      Just $
        UnqualifiedHsImport $
          HsImportModule "Prelude" Nothing
    }
  where
    s :: String
    s = TH.nameBase x.name

-- | Qualified import from "Prelude".
pQ :: PGlobal -> ResolvedName
pQ x = ResolvedName{
      string   = s
    , typ      = nameType s
    , hsImport =
      Just $
        QualifiedHsImport $
          HsImportModule "Prelude" (Just "P")
    }
  where
    s :: String
    s = TH.nameBase x.name

-- | Qualified import from "HsBindgen.Runtime.Internal.Prelude".
rQ :: RGlobal -> ResolvedName
rQ x = ResolvedName{
      string   = s
    , typ      = nameType s
    , hsImport =
      Just $
        QualifiedHsImport $
          HsImportModule "HsBindgen.Runtime.Internal.Prelude" (Just "RP")
    }
  where
    s :: String
    s = TH.nameBase x.name

-- | Unqualified import from "HsBindgen.Runtime.Internal.Prelude".
rU :: RGlobal -> ResolvedName
rU x = ResolvedName{
      string   = s
    , typ      = nameType s
    , hsImport =
      Just $
        UnqualifiedHsImport $
          HsImportModule "HsBindgen.Runtime.Internal.Prelude" Nothing
    }
  where
    s :: String
    s = TH.nameBase x.name

-- | Name type
data NameType =
    -- | An identifier, e.g., @foo@
    IdentifierName
    -- | An identifier with a magic hash at the end, e.g., @foo#@
  | IdentifierMagicHashName
    -- | An operator, e.g., @(+)@
  | OperatorName
  deriving (Eq, Ord, Show)

nameType :: String -> NameType
nameType nm
  | all isIdentChar nm
  = IdentifierName
    -- nm is non-empty because of the first guard
  | all isIdentChar (init nm) && isMagicHashChar (last nm)
  = IdentifierMagicHashName
  | otherwise
  = OperatorName
  where
    isMagicHashChar :: Char -> Bool
    isMagicHashChar c = c == '#'

    isIdentChar :: Char -> Bool
    isIdentChar c = Char.isAlphaNum c || c == '_' || c == '\''

-- TODO D: Remove 'moduleOf'.
-- | Create 'HsImportModule' from a definition module.
--
-- We need to map internal modules to external ones for @base@ names.
moduleOf :: String -> String -> HsImportModule
moduleOf "Void"       _ = HsImportModule "Data.Void" Nothing
moduleOf "CStringLen" _ =
    -- We want the same qualifier whether we get CStringLen from Foreign.C or
    -- GHC.Foreign, so special-case it here.
    HsImportModule "Foreign.C" (Just "FC")
moduleOf "NonEmpty" _ = HsImportModule "Data.List.NonEmpty" Nothing
moduleOf ":|"       _ = HsImportModule "Data.List.NonEmpty" Nothing
moduleOf "Nothing"  _ = HsImportModule "Data.Maybe" Nothing
moduleOf "Just"     _ = HsImportModule "Data.Maybe" Nothing
moduleOf ident m0
  | take 3 partsAll == ["HsBindgen","Runtime", "Internal"] =
    -- Do not replace "Internal" when treating @hs-bindgen-runtime@ modules.
      HsImportModule (Hs.moduleNameFromString m0) Nothing
  | otherwise = case partsNoInternal of
    ["C","Operator","Classes"]       -> HsImportModule "C.Expr.HostPlatform" (Just "C")
    ["GHC", "Bits"]                  -> HsImportModule "Data.Bits" (Just "Bits")
    -- See https://gitlab.haskell.org/ghc/ghc/-/issues/23212
    ["GHC", "Prim"]                  -> HsImportModule "GHC.Exts" Nothing
    ["GHC", "Base"]                  -> iPrelude
    ["GHC", "Classes"]               -> iPrelude
    ["GHC", "Show"]                  -> iPrelude
    ["GHC", "Types"]                 -> iPrelude
    -- - TH maps the module 'Text.Read' to 'GHC.Read'.
    -- - Not all functions of 'GHC.Read' are in Prelude.
    ["GHC", "Read"]                  -> if ident `Set.member` ghcReadInPrelude
                                        then iPrelude
                                        else HsImportModule "Text.Read" Nothing
    ["GHC", "Real"]                  -> iPrelude
    ["GHC", "Enum"]                  -> iPrelude
    ["GHC", "Float"]                 -> iPrelude
    ["GHC", "Num"]                   -> iPrelude
    ["GHC", "Maybe"]                 -> iPrelude
    ["GHC", "Ix"]                    -> HsImportModule "Data.Ix"   (Just "Ix")
    ("GHC" : "Foreign" : "C" : _)    -> HsImportModule "Foreign.C" (Just "FC")
    ("Foreign" : "C" : _)            -> HsImportModule "Foreign.C" (Just "FC")
    -- We'd prefer to use `Foreign.Ptr` because it is a stable and
    -- compiler-agnostic interface in contrast to `GHC.Ptr`. However,
    -- `Foreign.Ptr` does not export the constructor of `Ptr`, which we need.
    ["GHC", "Ptr"]                   -> HsImportModule "GHC.Ptr"   (Just "Ptr")
    ("GHC" : "Foreign" : _)          -> HsImportModule "Foreign"   (Just "F")
    ("Foreign" : _)                  -> HsImportModule "Foreign"   (Just "F")
    -- Since ghc-10, imports of Data.Proxy are for some reason converted to
    -- imports of GHC.Data.Proxy. For uniformity we'd prefer to use Data.Proxy
    -- regardless of the GHC version that is used to generate the bindings. That
    -- is why we replace the import name here:
    ["GHC", "Data", "Proxy"] ->
      HsImportModule "Data.Proxy" Nothing
    _ ->
      HsImportModule hsModuleNoInternal Nothing
  where
    partsAll = split '.' m0
    -- We drop "Internal" (to reduce ghc-internal migration noise)
    partsNoInternal = filter ("Internal" /=) partsAll
    hsModuleNoInternal = Hs.moduleNameFromString $ L.intercalate "." partsNoInternal

    ghcReadInPrelude :: Set String
    ghcReadInPrelude = Set.fromList ["Read"]

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
    -- sure to reserve the name in "HsBindgen.Backend.Hs.AST.Name".
    Tuple_type i        -> tupleResolvedName True  i
    Tuple_constructor i -> tupleResolvedName False i
    Applicative_pure    -> pU $ PGlobal 'pure
    Applicative_seq     -> pU $ PGlobal '(<*>)
    Maybe_just          -> pQ $ PGlobal 'Just
    Maybe_nothing       -> pQ $ PGlobal 'Nothing
    Monad_return        -> pU $ PGlobal 'return
    Monad_seq           -> pU $ PGlobal '(>>)

    -- Function pointers
    ToFunPtr_class        -> rQ $ RGlobal ''RP.ToFunPtr
    ToFunPtr_toFunPtr     -> rQ $ RGlobal  'RP.toFunPtr
    FromFunPtr_class      -> rQ $ RGlobal ''RP.FromFunPtr
    FromFunPtr_fromFunPtr -> rQ $ RGlobal  'RP.fromFunPtr

    -- Foreign function interface
    Foreign_Ptr             -> rQ $ RGlobal ''RP.Ptr
    Foreign_Ptr_constructor -> rQ $ RGlobal  'RP.Ptr
    Foreign_FunPtr          -> rQ $ RGlobal ''RP.FunPtr
    Foreign_plusPtr         -> rQ $ RGlobal  'RP.plusPtr
    Foreign_StablePtr       -> rQ $ RGlobal ''RP.StablePtr
    ConstantArray           -> rQ $ RGlobal ''RP.ConstantArray
    IncompleteArray         -> rQ $ RGlobal ''RP.IncompleteArray
    IO_type                 -> pU $ PGlobal ''IO
    CharValue_tycon         -> rQ $ RGlobal ''RP.CharValue
    CharValue_constructor   -> rQ $ RGlobal  'RP.CharValue
    CharValue_fromAddr      -> rQ $ RGlobal  'RP.charValueFromAddr
    Capi_with               -> rQ $ RGlobal  'RP.with
    Capi_allocaAndPeek      -> rQ $ RGlobal  'RP.allocaAndPeek
    Generic_class           -> rQ $ RGlobal ''RP.Generic

    -- StaticSize
    StaticSize_class           -> importQ ''HsBindgen.Runtime.Marshal.StaticSize
    StaticSize_staticSizeOf    -> importQ 'HsBindgen.Runtime.Marshal.staticSizeOf
    StaticSize_staticAlignment -> importQ 'HsBindgen.Runtime.Marshal.staticAlignment

    -- ReadRaw
    ReadRaw_class          -> importQ ''HsBindgen.Runtime.Marshal.ReadRaw
    ReadRaw_readRaw        -> importQ 'HsBindgen.Runtime.Marshal.readRaw
    ReadRaw_readRawByteOff -> importQ 'HsBindgen.Runtime.Marshal.readRawByteOff

    -- WriteRaw
    WriteRaw_class           -> importQ ''HsBindgen.Runtime.Marshal.WriteRaw
    WriteRaw_writeRaw        -> importQ 'HsBindgen.Runtime.Marshal.writeRaw
    WriteRaw_writeRawByteOff -> importQ 'HsBindgen.Runtime.Marshal.writeRawByteOff

    -- EquivStorable
    EquivStorable_type -> importQ ''HsBindgen.Runtime.Marshal.EquivStorable

    -- Storable
    Storable_class       -> importQ ''Foreign.Storable
    Storable_sizeOf      -> importQ 'Foreign.sizeOf
    Storable_alignment   -> importQ 'Foreign.alignment
    Storable_peekByteOff -> importQ 'Foreign.peekByteOff
    Storable_pokeByteOff -> importQ 'Foreign.pokeByteOff
    Storable_peek        -> importQ 'Foreign.peek
    Storable_poke        -> importQ 'Foreign.poke

    -- Flexible array members
    Flam_Offset_class  -> importQ ''HsBindgen.Runtime.FLAM.Offset
    Flam_Offset_offset -> importQ 'HsBindgen.Runtime.FLAM.offset
    WithFlam           -> importQ 'HsBindgen.Runtime.FLAM.WithFlam

    -- HasCField
    HasCField_class      -> importQ ''HsBindgen.Runtime.HasCField.HasCField
    HasCField_CFieldType -> importQ ''HsBindgen.Runtime.HasCField.CFieldType
    HasCField_offset#    -> importQ 'HsBindgen.Runtime.HasCField.offset#
    HasCField_fromPtr    -> importQ 'HsBindgen.Runtime.HasCField.fromPtr
    HasCField_peek       -> importQ 'HsBindgen.Runtime.HasCField.peek
    HasCField_poke       -> importQ 'HsBindgen.Runtime.HasCField.poke
    HasCField_readRaw    -> importQ 'HsBindgen.Runtime.HasCField.readRaw
    HasCField_writeRaw   -> importQ 'HsBindgen.Runtime.HasCField.writeRaw

    -- BitfieldPtr
    HasCBitfield_BitfieldPtr     -> importQ ''HsBindgen.Runtime.BitfieldPtr.BitfieldPtr

    -- HasCBitfield
    HasCBitfield_class           -> importQ ''HsBindgen.Runtime.HasCBitfield.HasCBitfield
    HasCBitfield_CBitfieldType   -> importQ ''HsBindgen.Runtime.HasCBitfield.CBitfieldType
    HasCBitfield_bitfieldOffset# -> importQ 'HsBindgen.Runtime.HasCBitfield.bitfieldOffset#
    HasCBitfield_bitfieldWidth#  -> importQ 'HsBindgen.Runtime.HasCBitfield.bitfieldWidth#
    HasCBitfield_toPtr           -> importQ 'HsBindgen.Runtime.HasCBitfield.toPtr
    HasCBitfield_peek            -> importQ 'HsBindgen.Runtime.HasCBitfield.peek
    HasCBitfield_poke            -> importQ 'HsBindgen.Runtime.HasCBitfield.poke

    -- HasField
    HasField_class    -> importQ ''GHC.Records.HasField
    HasField_getField -> importQ 'GHC.Records.getField

    -- Proxy
    Proxy_type        -> importQ ''Data.Proxy.Proxy
    Proxy_constructor -> importQ 'Data.Proxy.Proxy

    -- HasFFIType
    HasFFIType_class                 -> importQ ''HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    HasFFIType_fromFFIType           -> importQ 'HsBindgen.Runtime.Internal.HasFFIType.fromFFIType
    HasFFIType_toFFIType             -> importQ 'HsBindgen.Runtime.Internal.HasFFIType.toFFIType
    HasFFIType_castFunPtrFromFFIType -> importQ 'HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType
    HasFFIType_castFunPtrToFFIType   -> importQ 'HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType

    -- Functor
    Functor_fmap -> pU $ PGlobal 'fmap

    -- Unsafe
    IO_unsafePerformIO -> importQ 'System.IO.Unsafe.unsafePerformIO

    -- PtrConst
    PtrConst_type          -> importQ ''HsBindgen.Runtime.PtrConst.PtrConst
    PtrConst_unsafeFromPtr -> importQ 'HsBindgen.Runtime.PtrConst.unsafeFromPtr
    PtrConst_unsafeToPtr   -> importQ 'HsBindgen.Runtime.PtrConst.unsafeToPtr
    PtrConst_peek          -> importQ 'HsBindgen.Runtime.PtrConst.peek

    -- Prim
    Prim_class           -> importQ ''Primitive.Prim
    Prim_sizeOf#         -> importQ 'Primitive.sizeOf#
    Prim_alignment#      -> importQ 'Primitive.alignment#
    Prim_indexByteArray# -> importQ 'Primitive.indexByteArray#
    Prim_readByteArray#  -> importQ 'Primitive.readByteArray#
    Prim_writeByteArray# -> importQ 'Primitive.writeByteArray#
    Prim_indexOffAddr#   -> importQ 'Primitive.indexOffAddr#
    Prim_readOffAddr#    -> importQ 'Primitive.readOffAddr#
    Prim_writeOffAddr#   -> importQ 'Primitive.writeOffAddr#
    Prim_add#            -> importU '(GHC.Base.+#)
    Prim_mul#            -> importU '(GHC.Base.*#)

    Bitfield_class    -> importQ ''HsBindgen.Runtime.Internal.Bitfield.Bitfield
    Bits_class        -> importQ ''Data.Bits.Bits
    Bounded_class     -> importU ''Bounded
    Enum_class        -> pU $ PGlobal ''Enum
    Eq_class          -> pU $ PGlobal ''Eq
    FiniteBits_class  -> importU ''Data.Bits.FiniteBits
    Floating_class    -> pU $ PGlobal ''Floating
    Fractional_class  -> pU $ PGlobal ''Fractional
    Integral_class    -> pU $ PGlobal ''Integral
    Ix_class          -> importQ ''Data.Ix.Ix
    Num_class         -> pU $ PGlobal ''Num
    Ord_class         -> pU $ PGlobal ''Ord
    Read_class        -> pU $ PGlobal ''Read
    Read_readPrec     -> importQ 'Text.Read.readPrec
    Read_readList     -> importQ 'Text.Read.readList
    Read_readListPrec -> importQ 'Text.Read.readListPrec
    Real_class        -> pU $ PGlobal ''Real
    RealFloat_class   -> pU $ PGlobal ''RealFloat
    RealFrac_class    -> pU $ PGlobal ''RealFrac
    Show_class        -> pU $ PGlobal ''Show
    Show_showsPrec    -> pU $ PGlobal  'showsPrec

    -- We use @TyEq@ rather than @(~)@ because the latter is magical syntax on
    -- GHC-9.2. The use of @TyEq@ is uniform across GHC versions.
    NomEq_class -> importU ''HsBindgen.Runtime.Internal.TypeEquality.TyEq

    Not_class             -> importQ ''CExpr.Runtime.Not
    Not_not               -> importQ 'CExpr.Runtime.not
    Logical_class         -> importQ ''CExpr.Runtime.Logical
    Logical_and           -> importQ '(CExpr.Runtime.&&)
    Logical_or            -> importQ '(CExpr.Runtime.||)
    RelEq_class           -> importQ ''CExpr.Runtime.RelEq
    RelEq_eq              -> importQ '(CExpr.Runtime.==)
    RelEq_uneq            -> importQ '(CExpr.Runtime.!=)
    RelOrd_class          -> importQ ''CExpr.Runtime.RelOrd
    RelOrd_lt             -> importQ '(CExpr.Runtime.<)
    RelOrd_le             -> importQ '(CExpr.Runtime.<=)
    RelOrd_gt             -> importQ '(CExpr.Runtime.>)
    RelOrd_ge             -> importQ '(CExpr.Runtime.>=)
    Plus_class            -> importQ ''CExpr.Runtime.Plus
    Plus_resTyCon         -> importQ ''CExpr.Runtime.PlusRes
    Plus_plus             -> importQ 'CExpr.Runtime.plus
    Minus_class           -> importQ ''CExpr.Runtime.Minus
    Minus_resTyCon        -> importQ ''CExpr.Runtime.MinusRes
    Minus_negate          -> importQ 'CExpr.Runtime.negate
    Add_class             -> importQ ''CExpr.Runtime.Add
    Add_resTyCon          -> importQ ''CExpr.Runtime.AddRes
    Add_add               -> importQ '(CExpr.Runtime.+)
    Sub_class             -> importQ ''CExpr.Runtime.Sub
    Sub_resTyCon          -> importQ ''CExpr.Runtime.SubRes
    Sub_minus             -> importQ '(CExpr.Runtime.-)
    Mult_class            -> importQ ''CExpr.Runtime.Mult
    Mult_resTyCon         -> importQ ''CExpr.Runtime.MultRes
    Mult_mult             -> importQ '(CExpr.Runtime.*)
    Div_class             -> importQ ''CExpr.Runtime.Div
    Div_resTyCon          -> importQ ''CExpr.Runtime.DivRes
    Div_div               -> importQ '(CExpr.Runtime./)
    Rem_class             -> importQ ''CExpr.Runtime.Rem
    Rem_resTyCon          -> importQ ''CExpr.Runtime.RemRes
    Rem_rem               -> importQ '(CExpr.Runtime.%)
    Complement_class      -> importQ ''CExpr.Runtime.Complement
    Complement_resTyCon   -> importQ ''CExpr.Runtime.ComplementRes
    Complement_complement -> importQ '(CExpr.Runtime..~)
    Bitwise_class         -> importQ ''CExpr.Runtime.Bitwise
    Bitwise_resTyCon      -> importQ ''CExpr.Runtime.BitsRes
    Bitwise_and           -> importQ '(CExpr.Runtime..&.)
    Bitwise_or            -> importQ '(CExpr.Runtime..|.)
    Bitwise_xor           -> importQ '(CExpr.Runtime..^.)
    Shift_class           -> importQ ''CExpr.Runtime.Shift
    Shift_resTyCon        -> importQ ''CExpr.Runtime.ShiftRes
    Shift_shiftL          -> importQ '(CExpr.Runtime.<<)
    Shift_shiftR          -> importQ '(CExpr.Runtime.>>)

    GHC_Float_castWord32ToFloat  -> importQ 'GHC.Float.castWord32ToFloat
    GHC_Float_castWord64ToDouble -> importQ 'GHC.Float.castWord64ToDouble
    CFloat_constructor           -> importQ ''Foreign.C.CFloat
    CDouble_constructor          -> importQ ''Foreign.C.CDouble

    NonEmpty_constructor     -> importQ '(NonEmpty.:|)
    NonEmpty_singleton       -> importQ 'NonEmpty.singleton
    Map_fromList             -> importQ 'Map.fromList
    Read_readListDefault     -> importQ 'Text.Read.readListDefault
    Read_readListPrecDefault -> importQ 'Text.Read.readListPrecDefault

    CEnum_class                      -> importQ ''HsBindgen.Runtime.CEnum.CEnum
    CEnumZ_tycon                     -> importQ ''HsBindgen.Runtime.CEnum.CEnumZ
    CEnum_toCEnum                    -> importQ 'HsBindgen.Runtime.CEnum.toCEnum
    CEnum_fromCEnum                  -> importQ 'HsBindgen.Runtime.CEnum.fromCEnum
    CEnum_declaredValues             -> importQ 'HsBindgen.Runtime.CEnum.declaredValues
    CEnum_showsUndeclared            -> importQ 'HsBindgen.Runtime.CEnum.showsUndeclared
    CEnum_readPrecUndeclared         -> importQ 'HsBindgen.Runtime.CEnum.readPrecUndeclared
    CEnum_isDeclared                 -> importQ 'HsBindgen.Runtime.CEnum.isDeclared
    CEnum_mkDeclared                 -> importQ 'HsBindgen.Runtime.CEnum.mkDeclared
    SequentialCEnum_class            -> importQ ''HsBindgen.Runtime.CEnum.SequentialCEnum
    SequentialCEnum_minDeclaredValue -> importQ 'HsBindgen.Runtime.CEnum.minDeclaredValue
    SequentialCEnum_maxDeclaredValue -> importQ 'HsBindgen.Runtime.CEnum.maxDeclaredValue
    CEnum_declaredValuesFromList     -> importQ 'HsBindgen.Runtime.CEnum.declaredValuesFromList
    CEnum_showsCEnum                 -> importQ 'HsBindgen.Runtime.CEnum.shows
    CEnum_showsWrappedUndeclared     -> importQ 'HsBindgen.Runtime.CEnum.showsWrappedUndeclared
    CEnum_readPrecCEnum              -> importQ 'HsBindgen.Runtime.CEnum.readPrec
    CEnum_readPrecWrappedUndeclared  -> importQ 'HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared
    CEnum_seqIsDeclared              -> importQ 'HsBindgen.Runtime.CEnum.seqIsDeclared
    CEnum_seqMkDeclared              -> importQ 'HsBindgen.Runtime.CEnum.seqMkDeclared
    AsCEnum_type                     -> importQ ''HsBindgen.Runtime.CEnum.AsCEnum
    AsSequentialCEnum_type           -> importQ ''HsBindgen.Runtime.CEnum.AsSequentialCEnum

    ByteArray_type      -> importQ ''ByteArray
    SizedByteArray_type -> importQ ''HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray
    Block_type          -> importQ ''HsBindgen.Runtime.Block.Block

    ByteArray_getUnionPayload -> importQ 'HsBindgen.Runtime.Internal.ByteArray.getUnionPayload
    ByteArray_setUnionPayload -> importQ 'HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

    ComplexType -> importQ ''Complex.Complex

    Void_type       -> importU ''Data.Void.Void
    Unit_type       -> tupleResolvedName True 0
    CStringLen_type -> importQ ''Foreign.C.String.CStringLen
    CPtrdiff_type   -> importQ ''Foreign.C.CPtrdiff
    Char_type       -> pU $ PGlobal ''Char
    Int_type        -> pU $ PGlobal ''Int
    Double_type     -> pU $ PGlobal ''Double
    Float_type      -> pU $ PGlobal ''Float
    Bool_type       -> pU $ PGlobal ''Bool
    Int8_type       -> importQ ''Data.Int.Int8
    Int16_type      -> importQ ''Data.Int.Int16
    Int32_type      -> importQ ''Data.Int.Int32
    Int64_type      -> importQ ''Data.Int.Int64
    Word_type       -> importQ ''Word
    Word8_type      -> importQ ''Data.Word.Word8
    Word16_type     -> importQ ''Data.Word.Word16
    Word32_type     -> importQ ''Data.Word.Word32
    Word64_type     -> importQ ''Data.Word.Word64
    CChar_type      -> importQ ''Foreign.C.CChar
    CSChar_type     -> importQ ''Foreign.C.CSChar
    CUChar_type     -> importQ ''Foreign.C.CUChar
    CShort_type     -> importQ ''Foreign.C.CShort
    CUShort_type    -> importQ ''Foreign.C.CUShort
    CInt_type       -> importQ ''Foreign.C.CInt
    CUInt_type      -> importQ ''Foreign.C.CUInt
    CLong_type      -> importQ ''Foreign.C.CLong
    CULong_type     -> importQ ''Foreign.C.CULong
    CLLong_type     -> importQ ''Foreign.C.CLLong
    CULLong_type    -> importQ ''Foreign.C.CULLong
    CBool_type      -> importQ ''Foreign.C.CBool
    CFloat_type     -> importQ ''Foreign.C.CFloat
    CDouble_type    -> importQ ''Foreign.C.CDouble

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
