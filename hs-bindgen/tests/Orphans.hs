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

import HsBindgen.C.AST qualified as C
import HsBindgen.C.Tc.Macro as CMacro
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Name qualified as HsName
import HsBindgen.Hs.AST.Type qualified as HsType
import HsBindgen.Lib
import HsBindgen.NameHint
import HsBindgen.Runtime.Patterns

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
instance ToExpr C.SingleLoc
instance ToExpr C.Struct
instance ToExpr C.StructField
instance ToExpr C.TokenSpelling
instance ToExpr C.Type
instance ToExpr C.Typedef

instance ToExpr C.IntegerLiteral
instance ToExpr C.FloatingLiteral
instance ToExpr a => ToExpr (C.Range a)
instance ToExpr a => ToExpr (C.Token a)

-- Construct platform-independent expression
instance ToExpr C.SourcePath where
  toExpr = toExpr . splitDirectories . Text.unpack . C.getSourcePath

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

instance ToExpr C.QuantTy where
  toExpr quantTy = toExpr $ show quantTy

instance ToExpr (CMacro.ClassTyCon arity) where
  toExpr = \case
    CMacro.EqTyCon         -> Expr.App "EqTyCon"         []
    CMacro.OrdTyCon        -> Expr.App "OrdTyCon"        []
    CMacro.NumTyCon        -> Expr.App "NumTyCon"        []
    CMacro.IntegralTyCon   -> Expr.App "IntegralTyCon"   []
    CMacro.FractionalTyCon -> Expr.App "FractionalTyCon" []
    CMacro.DivTyCon        -> Expr.App "DivTyCon"        []
    CMacro.BitsTyCon       -> Expr.App "BitsTyCon"       []

instance ToExpr (DataTyCon n) where
  toExpr = \case
    BoolTyCon           -> Expr.App "BoolTyCon"      []
    StringTyCon         -> Expr.App "StringTyCon"    []
    IntLikeTyCon prim   -> Expr.App "IntLikeTyCon"   [toExpr prim]
    FloatLikeTyCon prim -> Expr.App "FloatLikeTyCon" [toExpr prim]
    PrimTyTyCon         -> Expr.App "PrimTyTyCon"    []
    EmptyTyCon          -> Expr.App "EmptyTyCon"     []

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
instance ToExpr (Hs.PeekByteOff ctx)
instance ToExpr (Hs.PhiType ctx)
instance ToExpr (Hs.PokeByteOff ctx)
instance ToExpr (t ctx) => ToExpr (Hs.Seq t ctx)
instance ToExpr Hs.StorableInstance
instance ToExpr (Hs.Struct n)
instance ToExpr Hs.StructOrigin
instance ToExpr (Hs.TauType ctx)
instance ToExpr Hs.TypeClass
instance ToExpr Hs.VarDecl
instance ToExpr (Hs.VarDeclRHS ctx)

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
    Hs.DeclInstance inst ->
      Expr.App "DeclInstance" [toExpr inst]
    Hs.DeclNewtypeInstance typeClass name ->
      Expr.App "DeclNewtypeInstance" [toExpr typeClass, toExpr name]
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
      ("forallTySize", toExpr forallTySize)
    , ("forallTyBinders", toExpr forallTyBinders)
    , ("forallTy", toExpr forallTy)
    ]

instance ToExpr Hs.VarDeclRHSAppHead where
  toExpr = \case
    Hs.InfixAppHead mfun ->
      Expr.App "InfixAppHead" [toExpr mfun]
    Hs.VarAppHead name ->
      Expr.App "VarAppHead" [toExpr name]

instance ToExpr (Hs.ClassTy ctx) where
  toExpr (Hs.ClassTy tycon args) =
    Expr.App "ClassTy" [toExpr tycon, toExpr args]

instance ToExpr (Hs.TyConAppTy ctx) where
  toExpr (Hs.TyConApp tycon args) =
    Expr.App "TyConApp" [toExpr tycon, toExpr args]

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
