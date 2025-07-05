{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.HsBindgen.Orphans.TreeDiff () where

import Data.Foldable (toList)
import Data.TreeDiff.Class (ToExpr (..))
import Data.TreeDiff.Expr qualified as Expr
import Data.TreeDiff.OMap qualified as OMap
import Data.Vec.Lazy (Vec)
import Data.Vec.Lazy qualified as Vec
import Foreign.C
import System.FilePath qualified as FilePath

import Clang.Enum.Simple
import Clang.HighLevel.Types qualified as C
import Clang.Paths qualified as Paths
import HsBindgen.BindingSpec.Internal qualified as BindingSpec
import HsBindgen.C.Reparse.Decl qualified as C
import HsBindgen.C.Tc.Macro qualified as CMacro
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Type qualified as HsType
import HsBindgen.Hs.Origin qualified as Origin
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint

import C.Char qualified as CExpr
import C.Type qualified as CExpr

import DeBruijn (Add, Idx, S, Size, addToInt, idxToInt, sizeToInt)

{-------------------------------------------------------------------------------
  base
-------------------------------------------------------------------------------}

instance ToExpr CInt where
  toExpr = toExpr . (fromIntegral :: CInt -> Int)

{-------------------------------------------------------------------------------
  hs-bindgen
-------------------------------------------------------------------------------}

-- Avoid references to system-dependent file locations
instance ToExpr Paths.SourcePath where
  toExpr = toExpr . FilePath.takeBaseName . Paths.getSourcePath

instance ToExpr Paths.CHeaderIncludePath where
  toExpr = toExpr . Paths.getCHeaderIncludePath

instance ToExpr (C.MTerm C.Ps)
instance ToExpr (C.XVar C.Ps)
instance ToExpr C.AnonId
instance ToExpr C.CheckedMacro
instance ToExpr C.CheckedMacroExpr
instance ToExpr C.CheckedMacroType
instance ToExpr C.CName
instance ToExpr C.Decl
instance ToExpr C.DeclInfo
instance ToExpr C.DeclKind
instance ToExpr C.DeclSpec
instance ToExpr C.Enum
instance ToExpr C.EnumConstant
instance ToExpr C.Function
instance ToExpr C.NameKind
instance ToExpr C.NameOrigin
instance ToExpr C.NamePair
instance ToExpr C.NewtypeNames
instance ToExpr C.PrimFloatType
instance ToExpr C.PrimIntType
instance ToExpr C.PrimSign
instance ToExpr C.PrimSignChar
instance ToExpr C.PrimType
instance ToExpr C.QualName
instance ToExpr C.RecordNames
instance ToExpr C.ResolvedExtBinding
instance ToExpr C.Struct
instance ToExpr C.StructField
instance ToExpr C.TranslationUnit
instance ToExpr C.Type
instance ToExpr C.Typedef
instance ToExpr C.TypedefRef
instance ToExpr C.Union
instance ToExpr C.UnionField

instance ToExpr BindingSpec.TypeSpec
instance ToExpr BindingSpec.InstanceSpec
instance ToExpr BindingSpec.StrategySpec
instance ToExpr BindingSpec.ConstraintSpec
instance ToExpr a => ToExpr (BindingSpec.Omittable a)

instance ToExpr CExpr.CharValue

instance ToExpr C.IntegerLiteral
instance ToExpr C.FloatingLiteral
instance ToExpr C.CharLiteral
instance ToExpr C.StringLiteral

-- do not use record syntax, as it's very verbose
instance ToExpr C.SingleLoc where
  toExpr (C.SingleLoc p l c) = toExpr $
    let filename = FilePath.takeFileName $ Paths.getSourcePath p
    in  filename ++ ":" ++ show l ++ ":" ++ show c

instance ToExpr Hs.HsModuleName
instance ToExpr Hs.HsIdentifier
instance ToExpr Hs.HsTypeClass
instance ToExpr Hs.ExtHsRef

instance ToExpr (C.MExpr C.Ps) where
  toExpr = \case
    C.MTerm tm ->
      Expr.App "MTerm" [toExpr tm]
    C.MApp _xapp fun args ->
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

instance ToExpr (CMacro.TyCon args resKi) where
  toExpr = \case
    CMacro.GenerativeTyCon tc  -> Expr.App "GenerativeTyCon" [toExpr tc]
    CMacro.FamilyTyCon     fam -> Expr.App "FamilyTyCon"     [toExpr fam]

instance ToExpr (CMacro.GenerativeTyCon args resKi) where
  toExpr = \case
    CMacro.DataTyCon  dc  -> Expr.App "DataTyCon"  [toExpr dc]
    CMacro.ClassTyCon cls -> Expr.App "ClassTyCon" [toExpr cls]

instance ToExpr (CMacro.DataTyCon n) where
  toExpr = \case
    CMacro.TupleTyCon n            -> Expr.App "TupleTyCon"     [toExpr n]
    CMacro.VoidTyCon               -> Expr.App "VoidTyCon"      []
    CMacro.PtrTyCon                -> Expr.App "PtrTyCon"       []
    CMacro.CharLitTyCon            -> Expr.App "CharLitTyCon"   []
    CMacro.IntLikeTyCon            -> Expr.App "IntLikeTyCon"   []
    CMacro.FloatLikeTyCon          -> Expr.App "FloatLikeTyCon" []
    CMacro.PrimIntInfoTyCon   info -> Expr.App "IntLikeTyCon"   [toExpr info]
    CMacro.PrimFloatInfoTyCon info -> Expr.App "FloatLikeTyCon" [toExpr info]
    CMacro.PrimTyTyCon             -> Expr.App "PrimTyTyCon"    []
    CMacro.EmptyTyCon              -> Expr.App "EmptyTyCon"     []

instance ToExpr CExpr.IntegralType where
instance ToExpr CExpr.CharLikeType where
instance ToExpr CExpr.IntLikeType where
instance ToExpr CExpr.Sign where
instance ToExpr CExpr.FloatingType where
instance ToExpr CMacro.IntegralType -- Note: different from CExpr.IntegralType

instance ToExpr (CMacro.FamilyTyCon n) where
  toExpr = \case
    CMacro.PlusResTyCon       -> Expr.App "PlusResTyCon"       []
    CMacro.MinusResTyCon      -> Expr.App "MinusResTyCon"      []
    CMacro.AddResTyCon        -> Expr.App "AddResTyCon"        []
    CMacro.SubResTyCon        -> Expr.App "SubResTyCon"        []
    CMacro.MultResTyCon       -> Expr.App "MultResTyCon"       []
    CMacro.DivResTyCon        -> Expr.App "DivResTyCon"        []
    CMacro.RemResTyCon        -> Expr.App "RemResTyCon"        []
    CMacro.ComplementResTyCon -> Expr.App "ComplementResTyCon" []
    CMacro.BitsResTyCon       -> Expr.App "BitsResTyCon"       []
    CMacro.ShiftResTyCon      -> Expr.App "ShiftResTyCon"      []

instance ToExpr HsType.HsType
instance ToExpr HsType.HsPrimType

instance ToExpr Hs.EmptyData
instance ToExpr Hs.Field
instance ToExpr Hs.ForeignImportDecl
instance ToExpr Hs.Newtype
instance ToExpr Hs.PatSyn
instance ToExpr Hs.StorableInstance
instance ToExpr t => ToExpr (Hs.Strategy t)
instance ToExpr Hs.VarDecl

instance ToExpr a => ToExpr (Origin.Decl a)
instance ToExpr Origin.EmptyData
instance ToExpr Origin.Field
instance ToExpr Origin.Newtype
instance ToExpr Origin.Struct
instance ToExpr Origin.PatSyn
instance ToExpr Origin.ForeignImport

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
    Hs.DeclInlineCInclude header ->
      Expr.App "DeclInlineCInclude" [toExpr header]
    Hs.DeclInlineC src ->
      Expr.App "DeclInlineC" [toExpr src]
    Hs.DeclForeignImport foreignImport ->
      Expr.App "DeclForeignImport" [toExpr foreignImport]
    Hs.DeclVar v ->
      Expr.App "DeclVar" [toExpr v]
    Hs.DeclUnionGetter u f n ->
      Expr.App "DeclUnionGetter" [toExpr u, toExpr f, toExpr n]
    Hs.DeclUnionSetter u f n ->
      Expr.App "DeclUnionSetter" [toExpr u, toExpr f, toExpr n]
    Hs.DeclSimple _d ->
      Expr.App "DeclSimple" [] -- TODO: no ToExpr SDecl

instance ToExpr a => ToExpr (Vec n a) where
  toExpr = Expr.Lst . map toExpr . Vec.toList

instance Hs.SingNamespace ns => ToExpr (Hs.HsName ns) where
  toExpr name = Expr.App "HsName" [
      toExpr ('@' : show (Hs.namespaceOf (Hs.singNamespace @ns)))
    , toExpr (Hs.getHsName name)
    ]

instance ToExpr Hs.InstanceDecl where
  toExpr = \case
    Hs.InstanceStorable struct inst ->
      Expr.App "InstanceStorable" [toExpr struct, toExpr inst]
    Hs.InstanceHasFLAM struct fty i ->
      Expr.App "InstanceHasFLAM" [toExpr struct, toExpr fty, toExpr i]
    Hs.InstanceCEnum struct typ vMap isSeq ->
      Expr.App "InstanceCEnum" [toExpr struct, toExpr typ, toExpr vMap, toExpr isSeq]
    Hs.InstanceSequentialCEnum struct minV maxV ->
      Expr.App "InstanceSequentialCEnum" [toExpr struct, toExpr minV, toExpr maxV]
    Hs.InstanceCEnumShow struct ->
      Expr.App "InstanceCEnumShow" [toExpr struct]
    Hs.InstanceCEnumRead struct ->
      Expr.App "InstanceCEnumRead" [toExpr struct]

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
  Declarations/declarators
-------------------------------------------------------------------------------}

instance ToExpr C.TypeName
instance ToExpr C.TypeSpecifierQualifier
instance ToExpr C.TypeSpecifier
instance ToExpr C.TypeQualifier
instance ToExpr C.AlignmentSpecifier
instance ToExpr C.StructOrUnionSpecifier
instance ToExpr C.StructOrUnion
instance ToExpr C.EnumSpecifier
instance ToExpr C.AttributeSpecifier
instance ToExpr C.Attribute
instance ToExpr C.AttributeToken
instance ToExpr C.BalancedToken
instance ToExpr C.DeclarationSpecifier
instance ToExpr C.StorageClassSpecifier
instance ToExpr C.FunctionSpecifier
instance ToExpr (C.Declarator C.Abstract)
instance ToExpr (C.DirectDeclarator C.Abstract)
instance ToExpr (C.ArrayDeclarator C.Abstract)
instance ToExpr (C.FunctionDeclarator C.Abstract)
instance ToExpr (C.Declarator C.Concrete)
instance ToExpr (C.DirectDeclarator C.Concrete)
instance ToExpr (C.ArrayDeclarator C.Concrete)
instance ToExpr (C.FunctionDeclarator C.Concrete)
instance ToExpr C.Pointers
instance ToExpr (C.DeclName C.Abstract)
instance ToExpr (C.DeclName C.Concrete)
instance ToExpr C.ArraySize
instance ToExpr C.Parameter
instance ToExpr C.ParameterDeclarator

instance ToExpr C.SizeExpression where
  toExpr (C.SizeExpression e _env) = toExpr e

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
