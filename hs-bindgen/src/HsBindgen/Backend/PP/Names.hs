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

import HsBindgen.SHs.AST
import HsBindgen.Hs.AST.Type

import Language.Haskell.TH.Syntax qualified as TH

import C.Expr.BuildPlatform qualified
import Data.Bits qualified
import Data.Ix qualified
import Data.Void qualified
import Foreign qualified
import Foreign.C qualified
import GHC.Float qualified
import HsBindgen.Runtime.ConstantArray qualified
import HsBindgen.Runtime.FlexibleArrayMember qualified
import HsBindgen.Runtime.Syntax qualified

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

-- | Name for @()@
unitResolvedName :: ResolvedName
unitResolvedName = ResolvedName "()" IdentifierName Nothing

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
moduleOf "Void" _  = HsImportModule "Data.Void" Nothing
moduleOf _ident m0 = case parts of
    ["C","Operator","Classes"]       -> HsImportModule "C.Expr.BuildPlatform" (Just "C")
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
    Unit_type            -> unitResolvedName
    Unit_constructor     -> unitResolvedName
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
    ConstantArray        -> importQ ''HsBindgen.Runtime.ConstantArray.ConstantArray
    IO_type              -> importU ''IO
    HasFlexibleArrayMember_class -> importQ ''HsBindgen.Runtime.FlexibleArrayMember.HasFlexibleArrayMember
    HasFlexibleArrayMember_offset -> importQ 'HsBindgen.Runtime.FlexibleArrayMember.flexibleArrayMemberOffset

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

    -- We now import ~ from Prelude;
    -- but it's not always there; it's also not in Data.Type.Equality
    -- on GHC-9.2, it just exists.
    --
    -- https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0371-non-magical-eq.md
    --
    -- So for now code using ~ will not work with preprocessor setup on GHC-9.2.
    NomEq_class -> ResolvedName "~" OperatorName (Just (UnqualifiedHsImport iPrelude))

    Not_class             -> importQ ''C.Expr.BuildPlatform.Not
    Not_not               -> importQ 'C.Expr.BuildPlatform.not
    Logical_class         -> importQ ''C.Expr.BuildPlatform.Logical
    Logical_and           -> importU '(C.Expr.BuildPlatform.&&)
    Logical_or            -> importU '(C.Expr.BuildPlatform.||)
    RelEq_class           -> importQ ''C.Expr.BuildPlatform.RelEq
    RelEq_eq              -> importU '(C.Expr.BuildPlatform.==)
    RelEq_uneq            -> importU '(C.Expr.BuildPlatform.!=)
    RelOrd_class          -> importQ ''C.Expr.BuildPlatform.RelOrd
    RelOrd_lt             -> importU '(C.Expr.BuildPlatform.<)
    RelOrd_le             -> importU '(C.Expr.BuildPlatform.<=)
    RelOrd_gt             -> importU '(C.Expr.BuildPlatform.>)
    RelOrd_ge             -> importU '(C.Expr.BuildPlatform.>=)
    Plus_class            -> importQ ''C.Expr.BuildPlatform.Plus
    Plus_resTyCon         -> importQ ''C.Expr.BuildPlatform.PlusRes
    Plus_plus             -> importQ 'C.Expr.BuildPlatform.plus
    Minus_class           -> importQ ''C.Expr.BuildPlatform.Minus
    Minus_resTyCon        -> importQ ''C.Expr.BuildPlatform.MinusRes
    Minus_negate          -> importQ 'C.Expr.BuildPlatform.negate
    Add_class             -> importQ ''C.Expr.BuildPlatform.Add
    Add_resTyCon          -> importQ ''C.Expr.BuildPlatform.AddRes
    Add_add               -> importU '(C.Expr.BuildPlatform.+)
    Sub_class             -> importQ ''C.Expr.BuildPlatform.Sub
    Sub_resTyCon          -> importQ ''C.Expr.BuildPlatform.SubRes
    Sub_minus             -> importU '(C.Expr.BuildPlatform.-)
    Mult_class            -> importQ ''C.Expr.BuildPlatform.Mult
    Mult_resTyCon         -> importQ ''C.Expr.BuildPlatform.MultRes
    Mult_mult             -> importU '(C.Expr.BuildPlatform.*)
    Div_class             -> importQ ''C.Expr.BuildPlatform.Div
    Div_resTyCon          -> importQ ''C.Expr.BuildPlatform.DivRes
    Div_div               -> importU '(C.Expr.BuildPlatform./)
    Rem_class             -> importQ ''C.Expr.BuildPlatform.Rem
    Rem_resTyCon          -> importQ ''C.Expr.BuildPlatform.RemRes
    Rem_rem               -> importU '(C.Expr.BuildPlatform.%)
    Complement_class      -> importQ ''C.Expr.BuildPlatform.Complement
    Complement_resTyCon   -> importQ ''C.Expr.BuildPlatform.ComplementRes
    Complement_complement -> importQ '(C.Expr.BuildPlatform..~)
    Bitwise_class         -> importQ ''C.Expr.BuildPlatform.Bitwise
    Bitwise_resTyCon      -> importQ ''C.Expr.BuildPlatform.BitsRes
    Bitwise_and           -> importU '(C.Expr.BuildPlatform..&.)
    Bitwise_or            -> importU '(C.Expr.BuildPlatform..|.)
    Bitwise_xor           -> importU '(C.Expr.BuildPlatform..^.)
    Shift_class           -> importQ ''C.Expr.BuildPlatform.Shift
    Shift_resTyCon        -> importQ ''C.Expr.BuildPlatform.ShiftRes
    Shift_shiftL          -> importU '(C.Expr.BuildPlatform.<<)
    Shift_shiftR          -> importU '(C.Expr.BuildPlatform.>>)

    IntLike_tycon    -> importQ ''HsBindgen.Runtime.Syntax.IntLike
    FloatLike_tycon  -> importQ ''HsBindgen.Runtime.Syntax.FloatLike

    GHC_Float_castWord32ToFloat -> importQ 'GHC.Float.castWord32ToFloat
    GHC_Float_castWord64ToDouble -> importQ 'GHC.Float.castWord64ToDouble
    CFloat_constructor -> importQ ''Foreign.C.CFloat
    CDouble_constructor -> importQ ''Foreign.C.CDouble

    PrimType hsPrimType  -> case hsPrimType of
      HsPrimVoid     -> importU ''Data.Void.Void
      HsPrimUnit     -> unitResolvedName
      HsPrimCChar    -> importQ ''Foreign.C.CChar
      HsPrimCSChar   -> importQ ''Foreign.C.CSChar
      HsPrimCUChar   -> importQ ''Foreign.C.CUChar
      HsPrimCInt     -> importQ ''Foreign.C.CInt
      HsPrimCUInt    -> importQ ''Foreign.C.CUInt
      HsPrimCShort   -> importQ ''Foreign.C.CShort
      HsPrimCUShort  -> importQ ''Foreign.C.CUShort
      HsPrimCLong    -> importQ ''Foreign.C.CLong
      HsPrimCULong   -> importQ ''Foreign.C.CULong
      HsPrimCLLong   -> importQ ''Foreign.C.CLLong
      HsPrimCULLong  -> importQ ''Foreign.C.CULLong
      HsPrimCBool    -> importQ ''Foreign.C.CBool
      HsPrimCFloat   -> importQ ''Foreign.C.CFloat
      HsPrimCDouble  -> importQ ''Foreign.C.CDouble
      HsPrimCPtrDiff -> importQ ''Foreign.C.CPtrdiff
      HsPrimInt      -> importU ''Int

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
