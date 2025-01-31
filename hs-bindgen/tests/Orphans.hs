{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Orphans where

import Data.Foldable (toList)
import Data.Text qualified as Text
import Data.TreeDiff.Class (ToExpr(..))
import Data.TreeDiff.Expr qualified as Expr
import Data.TreeDiff.OMap qualified as OMap
import Data.Vec.Lazy (Vec)
import Data.Vec.Lazy qualified as Vec
import Foreign.C
import System.FilePath (splitDirectories)
import System.FilePath.Posix qualified as Posix

import HsBindgen.C.AST qualified as C
import HsBindgen.C.Tc.Macro as CMacro
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Name qualified as HsName
import HsBindgen.Hs.AST.Type qualified as HsType
import HsBindgen.Lib
import HsBindgen.NameHint
import HsBindgen.Runtime.Enum.Simple

import C.Type qualified as CExpr

import DeBruijn

{-------------------------------------------------------------------------------
  base
-------------------------------------------------------------------------------}

instance ToExpr CInt where
  toExpr = toExpr . (fromIntegral :: CInt -> Int)

{-------------------------------------------------------------------------------
  hs-bindgen
-------------------------------------------------------------------------------}

instance ToExpr CHeader

instance ToExpr C.Attribute
instance ToExpr C.CName
instance ToExpr C.Decl
instance ToExpr C.DeclName
instance ToExpr C.DeclPath
instance ToExpr C.Enu
instance ToExpr C.EnumValue
instance ToExpr C.Function
instance ToExpr C.Header
instance ToExpr C.Macro
instance ToExpr C.MacroDecl
instance ToExpr C.MTerm
instance ToExpr C.MultiLoc
instance ToExpr C.OpaqueEnum
instance ToExpr C.OpaqueStruct
instance ToExpr C.PrimFloatType
instance ToExpr C.PrimIntType
instance ToExpr C.PrimSign
instance ToExpr C.PrimType
instance ToExpr C.Struct
instance ToExpr C.StructField
instance ToExpr C.TokenSpelling
instance ToExpr C.Type
instance ToExpr C.Typedef

instance ToExpr C.IntegerLiteral
instance ToExpr C.FloatingLiteral
instance ToExpr a => ToExpr (C.Range a)
instance ToExpr a => ToExpr (C.Token a)

-- do not use record syntax, as it's very verbose
instance ToExpr C.SingleLoc where
  toExpr (C.SingleLoc p l c) = toExpr $
    -- use posix directory separators even on windows
    Posix.joinPath (splitDirectories (Text.unpack (C.getSourcePath p))) ++ ":" ++
    show l ++ ":" ++
    show c

instance ToExpr C.ReparseError where
  toExpr C.ReparseError {..} = Expr.Rec "ReparseError" $ OMap.fromList
    [ ("reparseError", toExpr $ f reparseError)
    , ("reparseErrorTokens", toExpr reparseErrorTokens)
    ]
    where
      -- reparseError may contain paths
      f :: String -> String
      f ('\\' : '\\' : xs) = '/' : f xs
      f ('\\' : xs)        = '/' : f xs
      f (c : xs)           = c   : f xs
      f []                 = []

instance ToExpr C.TcMacroError where
  toExpr err = toExpr $ C.pprTcMacroError err

instance ToExpr C.MExpr where
  toExpr = \case
    C.MTerm tm ->
      Expr.App "MTerm" [toExpr tm]
    C.MApp fun args ->
      Expr.App "MApp" [toExpr fun, toExpr (toList args)]

instance ToExpr ( C.MFun arity ) where
  toExpr f = Expr.App (show f) []

instance Show e => ToExpr ( CMacro.Quant e ) where
  toExpr quantTy = toExpr $ show quantTy

instance ToExpr (CMacro.ClassTyCon arity) where
  toExpr = \case
    CMacro.NotTyCon        -> Expr.App "NotTyCon"        []
    CMacro.LogicalTyCon    -> Expr.App "LogicalTyCon"    []
    CMacro.RelEqTyCon      -> Expr.App "RelEqTyCon"      []
    CMacro.RelOrdTyCon     -> Expr.App "RelOrdTyCon"     []
    CMacro.PlusTyCon       -> Expr.App "PlusTyCon"       []
    CMacro.MinusTyCon      -> Expr.App "MinusTyCon"      []
    CMacro.AddTyCon        -> Expr.App "AddTyCon"        []
    CMacro.SubTyCon        -> Expr.App "SubTyCon"        []
    CMacro.MultTyCon       -> Expr.App "MultTyCon"       []
    CMacro.DivTyCon        -> Expr.App "DivTyCon"        []
    CMacro.RemTyCon        -> Expr.App "RemTyCon"        []
    CMacro.ComplementTyCon -> Expr.App "ComplementTyCon" []
    CMacro.BitwiseTyCon    -> Expr.App "BitwiseTyCon"    []
    CMacro.ShiftTyCon      -> Expr.App "ShiftTyCon"      []


instance ToExpr (TyCon args resKi) where
  toExpr = \case
    GenerativeTyCon tc  -> Expr.App "GenerativeTyCon" [toExpr tc]
    FamilyTyCon     fam -> Expr.App "FamilyTyCon"     [toExpr fam]

instance ToExpr (GenerativeTyCon args resKi) where
  toExpr = \case
    DataTyCon  dc  -> Expr.App "DataTyCon"  [toExpr dc]
    ClassTyCon cls -> Expr.App "ClassTyCon" [toExpr cls]

instance ToExpr (DataTyCon n) where
  toExpr = \case
    VoidTyCon               -> Expr.App "VoidTyCon"      []
    PtrTyCon                -> Expr.App "PtrTyCon"       []
    StringTyCon             -> Expr.App "StringTyCon"    []
    IntLikeTyCon            -> Expr.App "IntLikeTyCon"   []
    FloatLikeTyCon          -> Expr.App "FloatLikeTyCon" []
    PrimIntInfoTyCon   info -> Expr.App "IntLikeTyCon"   [toExpr info]
    PrimFloatInfoTyCon info -> Expr.App "FloatLikeTyCon" [toExpr info]
    PrimTyTyCon             -> Expr.App "PrimTyTyCon"    []
    EmptyTyCon              -> Expr.App "EmptyTyCon"     []

instance ToExpr CExpr.IntegralType where
  toExpr = \case
    CExpr.Bool       -> Expr.App "Bool"     []
    CExpr.CharLike s -> Expr.App "CharLike" [toExpr s]
    CExpr.IntLike  i -> Expr.App "IntLike"  [toExpr i]
instance ToExpr CExpr.CharLikeType where
  toExpr = \case
    CExpr.Char  -> Expr.App "Char" []
    CExpr.SChar -> Expr.App "SChar" []
    CExpr.UChar -> Expr.App "UChar" []
instance ToExpr CExpr.IntLikeType where
  toExpr = \case
    CExpr.Short    s -> Expr.App "Short"    [toExpr s]
    CExpr.Int      s -> Expr.App "Int"      [toExpr s]
    CExpr.Long     s -> Expr.App "Long"     [toExpr s]
    CExpr.LongLong s -> Expr.App "LongLong" [toExpr s]
    CExpr.PtrDiff    -> Expr.App "PtrDiff"  []
instance ToExpr CExpr.Sign where
  toExpr = \case
    CExpr.Signed   -> Expr.App "Signed"   []
    CExpr.Unsigned -> Expr.App "Unsigned" []

instance ToExpr CExpr.FloatingType where
  toExpr = \case
    CExpr.FloatType  -> Expr.App "FloatType"  []
    CExpr.DoubleType -> Expr.App "DoubleType" []

instance ToExpr (FamilyTyCon n) where
  toExpr = \case
    PlusResTyCon       -> Expr.App "PlusResTyCon"       []
    MinusResTyCon      -> Expr.App "MinusResTyCon"      []
    AddResTyCon        -> Expr.App "AddResTyCon"        []
    SubResTyCon        -> Expr.App "SubResTyCon"        []
    MultResTyCon       -> Expr.App "MultResTyCon"       []
    DivResTyCon        -> Expr.App "DivResTyCon"        []
    RemResTyCon        -> Expr.App "RemResTyCon"        []
    ComplementResTyCon -> Expr.App "ComplementResTyCon" []
    BitsResTyCon       -> Expr.App "BitsResTyCon"       []
    ShiftResTyCon      -> Expr.App "ShiftResTyCon"      []

instance ToExpr HsType.HsType
instance ToExpr HsType.HsPrimType

instance ToExpr Hs.Field
instance ToExpr Hs.FieldOrigin
instance ToExpr Hs.ForeignImportDecl
instance ToExpr Hs.ForeignImportDeclOrigin
instance ToExpr Hs.Newtype
instance ToExpr Hs.NewtypeOrigin
instance ToExpr Hs.PatSyn
instance ToExpr Hs.PatSynOrigin
instance ToExpr Hs.StorableInstance
instance ToExpr Hs.Strategy
instance ToExpr Hs.StructOrigin
instance ToExpr Hs.TypeClass
instance ToExpr Hs.VarDecl

instance ToExpr (Hs.PeekByteOff ctx)
instance ToExpr (Hs.PhiType ctx)
instance ToExpr (Hs.PokeByteOff ctx)
instance ToExpr (Hs.Struct n)
instance ToExpr (Hs.TauType ctx)
instance ToExpr (Hs.VarDeclRHS ctx)
instance ToExpr (t ctx) => ToExpr (Hs.Seq t ctx)

instance ToExpr Hs.Decl where
  toExpr = \case
    Hs.DeclData struct ->
      Expr.App "DeclData" [toExpr struct]
    Hs.DeclEmpty name ->
      Expr.App "DeclEmpty" [toExpr name]
    Hs.DeclNewtype nt ->
      Expr.App "DeclNewtype" [toExpr nt]
    Hs.DeclPatSyn patSyn ->
      Expr.App "DeclPatSyn" [toExpr patSyn]
    Hs.DeclDefineInstance inst ->
      Expr.App "DeclInstance" [toExpr inst]
    Hs.DeclDeriveInstance strategy typeClass name ->
      Expr.App "DeclNewtypeInstance" [toExpr strategy, toExpr typeClass, toExpr name]
    Hs.DeclForeignImport foreignImport ->
      Expr.App "DeclForeignImport" [toExpr foreignImport]
    Hs.DeclVar v ->
      Expr.App "DeclVar" [toExpr v]

instance ToExpr a => ToExpr (Vec n a) where
  toExpr = Expr.Lst . map toExpr . Vec.toList

instance HsName.SingNamespace ns => ToExpr (HsName.HsName ns) where
  toExpr name = Expr.App "HsName" [
      toExpr ('@' : show (HsName.namespaceOf (HsName.singNamespace @ns)))
    , toExpr (HsName.getHsName name)
    ]

instance ToExpr Hs.InstanceDecl where
  toExpr = \case
    Hs.InstanceStorable struct inst ->
      Expr.App "InstanceStorable" [toExpr struct, toExpr inst]
    Hs.InstanceHasFLAM struct fty i ->
      Expr.App "InstanceHasFLAM" [toExpr struct, toExpr fty, toExpr i]

instance ToExpr (t (S ctx)) => ToExpr (Hs.Lambda t ctx) where
  toExpr (Hs.Lambda name body) = Expr.App "Lambda" [toExpr name, toExpr body]

instance (ToExpr (pure ctx), ToExpr (xs ctx)) => ToExpr (Hs.Ap pure xs ctx)

instance (forall ctx'. ToExpr (t ctx')) => ToExpr (Hs.ElimStruct t ctx) where
  toExpr (Hs.ElimStruct idx struct add t) =
    Expr.App "ElimStruct" [toExpr idx, toExpr struct, toExpr add, toExpr t]

instance ToExpr NameHint where
  toExpr (NameHint s) = Expr.App "NameHint" [toExpr s]

instance ToExpr (Hs.StructCon ctx) where
  toExpr (Hs.StructCon struct) = Expr.App "StructCon" [toExpr struct]

instance ToExpr Hs.SigmaType where
  toExpr Hs.ForallTy {..} = Expr.Rec "ForallTy" $ OMap.fromList [
      ("forallTyBinders", toExpr forallTyBinders)
    , ("forallTy", toExpr forallTy)
    ]

instance ToExpr Hs.VarDeclRHSAppHead where
  toExpr = \case
    Hs.InfixAppHead mfun ->
      Expr.App "InfixAppHead" [toExpr mfun]
    Hs.VarAppHead name ->
      Expr.App "VarAppHead" [toExpr name]

instance ToExpr (Hs.PredType ctx) where
  toExpr = \case
    Hs.DictTy cls args ->
      Expr.App "ClassTy" [toExpr cls, toExpr args]
    Hs.NomEqTy l r ->
      Expr.App "NomEqTy" [toExpr l, toExpr r]

instance ToExpr Hs.ATyCon where
  toExpr (Hs.ATyCon tc) =
    Expr.App "ATyCon" [toExpr tc]

instance ToExpr Hs.AClass where
  toExpr (Hs.AClass tc) =
    Expr.App "AClass" [toExpr tc]

{-------------------------------------------------------------------------------
  hs-bindgen-runtime
-------------------------------------------------------------------------------}

instance ToExpr (SimpleEnum hs)

{-------------------------------------------------------------------------------
  DeBruijn
-------------------------------------------------------------------------------}

instance ToExpr (Add n m p) where
  toExpr add = Expr.App "Add" [toExpr (addToInt add)]

instance ToExpr (Idx j) where
  toExpr idx = Expr.App "Idx" [toExpr (idxToInt idx)]

instance ToExpr (Size ctx) where
  toExpr size = Expr.App "Size" [toExpr (sizeToInt size)]
