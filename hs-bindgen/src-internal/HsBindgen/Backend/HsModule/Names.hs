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
import Data.Ix qualified
import Data.List qualified as L
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Void qualified
import Foreign qualified
import Foreign.C qualified
import Foreign.C.String qualified
import GHC.Float qualified
import GHC.Ptr qualified
import Language.Haskell.TH.Syntax qualified as TH
import System.IO.Unsafe qualified
import Text.Read qualified

import C.Char qualified as CExpr.Runtime
import C.Expr.HostPlatform qualified as CExpr.Runtime

import HsBindgen.Runtime.Bitfield qualified
import HsBindgen.Runtime.Block qualified
import HsBindgen.Runtime.ByteArray qualified
import HsBindgen.Runtime.CAPI qualified
import HsBindgen.Runtime.CEnum qualified
import HsBindgen.Runtime.ConstantArray qualified
import HsBindgen.Runtime.FlexibleArrayMember qualified
import HsBindgen.Runtime.IncompleteArray qualified
import HsBindgen.Runtime.Marshal qualified
import HsBindgen.Runtime.SizedByteArray qualified

import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.SHs.AST
import HsBindgen.Imports

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
  -- We want the same qualifier whether we get CStringLen from Foreign.C or
  -- GHC.Foreign, so special-case it here.
  HsImportModule "Foreign.C" (Just "FC")
moduleOf "NonEmpty" _ = HsImportModule "Data.List.NonEmpty" Nothing
moduleOf ":|"       _ = HsImportModule "Data.List.NonEmpty" Nothing
moduleOf ident m0 = case parts of
    ["C","Operator","Classes"]       -> HsImportModule "C.Expr.HostPlatform" (Just "C")
    ["GHC", "Bits"]                  -> HsImportModule "Data.Bits" (Just "Bits")
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

    -- otherwise just use module as is.
    _ -> HsImportModule (L.intercalate "." parts) Nothing
  where
    -- we drop "Internal" (to reduce ghc-internal migration noise)
    parts = filter ("Internal" /=) (split '.' m0)
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
    Tuple_type i                  -> tupleResolvedName True  i
    Tuple_constructor i           -> tupleResolvedName False i
    Applicative_pure              -> importU 'pure
    Applicative_seq               -> importU '(<*>)
    Monad_return                  -> importU 'return
    Monad_seq                     -> importU '(>>)
    StaticSize_class              -> importQ ''HsBindgen.Runtime.Marshal.StaticSize
    ReadRaw_class                 -> importQ ''HsBindgen.Runtime.Marshal.ReadRaw
    WriteRaw_class                -> importQ ''HsBindgen.Runtime.Marshal.WriteRaw

    -- TODO: If we use the TH resolution mechanism it is going to pick up
    -- HsBindgen.Runtime.FunPtr.Class which is not exposed and leads to
    -- generated code not compiling. So we have to construct the ResolvedName
    -- records by hand here.
    --
    -- However, once #1061 is addressed this should no longer be a problem
    --
    ToFunPtr_class                -> let s = "ToFunPtr"
                                         m = Just "HsBindgen.Runtime.FunPtr"
                                      in ResolvedName
                                          { resolvedNameString = s
                                          , resolvedNameType   = nameType s
                                          , resolvedNameImport = fmap (QualifiedHsImport . moduleOf s) m
                                          }
    ToFunPtr_toFunPtr             -> let s = "toFunPtr"
                                         m = Just "HsBindgen.Runtime.FunPtr"
                                      in ResolvedName
                                          { resolvedNameString = s
                                          , resolvedNameType   = nameType s
                                          , resolvedNameImport = fmap (QualifiedHsImport . moduleOf s) m
                                          }
    FromFunPtr_class              -> let s = "FromFunPtr"
                                         m = Just "HsBindgen.Runtime.FunPtr"
                                      in ResolvedName
                                          { resolvedNameString = s
                                          , resolvedNameType   = nameType s
                                          , resolvedNameImport = fmap (QualifiedHsImport . moduleOf s) m
                                          }
    FromFunPtr_fromFunPtr         -> let s = "fromFunPtr"
                                         m = Just "HsBindgen.Runtime.FunPtr"
                                      in ResolvedName
                                          { resolvedNameString = s
                                          , resolvedNameType   = nameType s
                                          , resolvedNameImport = fmap (QualifiedHsImport . moduleOf s) m
                                          }

    Storable_class                -> importQ ''Foreign.Storable
    Storable_sizeOf               -> importQ 'Foreign.sizeOf
    Storable_alignment            -> importQ 'Foreign.alignment
    Storable_peekByteOff          -> importQ 'Foreign.peekByteOff
    Storable_pokeByteOff          -> importQ 'Foreign.pokeByteOff
    Storable_peek                 -> importQ 'Foreign.peek
    Storable_poke                 -> importQ 'Foreign.poke
    Foreign_Ptr                   -> importQ ''Foreign.Ptr
    Foreign_FunPtr                -> importQ ''Foreign.FunPtr
    Ptr_constructor               -> importQ ''GHC.Ptr.Ptr
    ConstantArray                 -> importQ ''HsBindgen.Runtime.ConstantArray.ConstantArray
    IncompleteArray               -> importQ ''HsBindgen.Runtime.IncompleteArray.IncompleteArray
    IO_type                       -> importU ''IO
    HasFlexibleArrayMember_class  -> importQ ''HsBindgen.Runtime.FlexibleArrayMember.HasFlexibleArrayMember
    HasFlexibleArrayMember_offset -> importQ 'HsBindgen.Runtime.FlexibleArrayMember.flexibleArrayMemberOffset
    Bitfield_peekBitOffWidth      -> importQ 'HsBindgen.Runtime.Bitfield.peekBitOffWidth
    Bitfield_pokeBitOffWidth      -> importQ 'HsBindgen.Runtime.Bitfield.pokeBitOffWidth
    CharValue_tycon               -> importQ ''CExpr.Runtime.CharValue
    CharValue_constructor         -> importQ 'CExpr.Runtime.CharValue
    CharValue_fromAddr            -> importQ 'CExpr.Runtime.charValueFromAddr
    CAPI_with                     -> importQ 'Foreign.with
    CAPI_allocaAndPeek            -> importQ 'HsBindgen.Runtime.CAPI.allocaAndPeek
    ConstantArray_withPtr         -> importQ 'HsBindgen.Runtime.ConstantArray.withPtr
    IncompleteArray_withPtr       -> importQ 'HsBindgen.Runtime.IncompleteArray.withPtr

    -- Unsafe
    IO_unsafePerformIO -> importQ 'System.IO.Unsafe.unsafePerformIO

    Bits_class        -> importQ ''Data.Bits.Bits
    Bounded_class     -> importU ''Bounded
    Enum_class        -> importU ''Enum
    Eq_class          -> importU ''Eq
    FiniteBits_class  -> importU ''Data.Bits.FiniteBits
    Floating_class    -> importU ''Floating
    Fractional_class  -> importU ''Fractional
    Integral_class    -> importU ''Integral
    Ix_class          -> importQ ''Data.Ix.Ix
    Num_class         -> importU ''Num
    Ord_class         -> importU ''Ord
    Read_class        -> importU ''Read
    Read_readPrec     -> importQ 'Text.Read.readPrec
    Read_readList     -> importQ 'Text.Read.readList
    Read_readListPrec -> importQ 'Text.Read.readListPrec
    Real_class        -> importU ''Real
    RealFloat_class   -> importU ''RealFloat
    RealFrac_class    -> importU ''RealFrac
    Show_class        -> importU ''Show
    Show_showsPrec    -> importU 'showsPrec

    -- We now import ~ from Prelude;
    -- but it's not always there; it's also not in Data.Type.Equality
    -- on GHC-9.2, it just exists.
    --
    -- https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0371-non-magical-eq.md
    --
    -- So for now code using ~ will not work with preprocessor setup on GHC-9.2.
    NomEq_class -> ResolvedName "~" OperatorName (Just (UnqualifiedHsImport iPrelude))

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
    CEnum_showsCEnum                 -> importQ 'HsBindgen.Runtime.CEnum.showsCEnum
    CEnum_showsWrappedUndeclared     -> importQ 'HsBindgen.Runtime.CEnum.showsWrappedUndeclared
    CEnum_readPrecCEnum              -> importQ 'HsBindgen.Runtime.CEnum.readPrecCEnum
    CEnum_readPrecWrappedUndeclared  -> importQ 'HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared
    CEnum_seqIsDeclared              -> importQ 'HsBindgen.Runtime.CEnum.seqIsDeclared
    CEnum_seqMkDeclared              -> importQ 'HsBindgen.Runtime.CEnum.seqMkDeclared
    AsCEnum_type                     -> importQ ''HsBindgen.Runtime.CEnum.AsCEnum
    AsSequentialCEnum_type           -> importQ ''HsBindgen.Runtime.CEnum.AsSequentialCEnum

    ByteArray_type      -> importQ ''ByteArray
    SizedByteArray_type -> importQ ''HsBindgen.Runtime.SizedByteArray.SizedByteArray
    Block_type          -> importQ ''HsBindgen.Runtime.Block.Block

    ByteArray_getUnionPayload -> importQ 'HsBindgen.Runtime.ByteArray.getUnionPayload
    ByteArray_setUnionPayload -> importQ 'HsBindgen.Runtime.ByteArray.setUnionPayload

    PrimType hsPrimType -> case hsPrimType of
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
      HsPrimCSize      -> importQ ''Foreign.C.CSize
      HsPrimCStringLen -> importQ ''Foreign.C.String.CStringLen
      HsPrimInt        -> importU ''Int

    ComplexType -> importQ ''Complex.Complex

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
